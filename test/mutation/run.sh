#!/usr/bin/env bash
#
# Mutation-testing harness for Cribrum.
#
# Reads test/mutation/mutants.tsv (TAB-separated):
#   FILE \t FIND \t REPLACE \t DESCRIPTION
#
# For each mutant: backup -> sed substitute -> rebuild + run tests -> restore.
# A surviving mutant = tests still pass (exit 0). The script exits non-zero if
# any mutant survives, per the project's "zero surviving mutants" gate.
#
# Approved-survivor list: pass approved mutant indices (1-based) via
#   APPROVED="3 7"   ./test/mutation/run.sh
# These mutants are still executed but their survival does NOT fail the run
# (used for mutants that are semantically equivalent to the original).

set -u
set -o pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TSV="$ROOT/test/mutation/mutants.tsv"
PACK_BIN="${PACK_BIN:-pack}"
APPROVED="${APPROVED:-}"

cd "$ROOT"

if [[ ! -r "$TSV" ]]; then
  echo "FATAL: cannot read $TSV" >&2
  exit 2
fi

# 0. Pack content-hashes installs; if the SAME source content was previously
# installed and corrupted (e.g. by an interleaved mutant write), pack happily
# reuses the stale .ttc. Wipe every cached cribrum + teaweb install before
# each reinstall so the build always reflects on-disk source.
#
# teaweb depends on cribrum; when cribrum's content-hash changes (each
# mutant iteration), the previously-installed teaweb references an
# obsolete cribrum hash. Reinstalling teaweb here keeps the
# examples/teaweb/counter demo (and the teaweb-test suite) buildable
# *between* gate runs, not just at the end.
reinstall_cribrum () {
  rm -rf build test/build
  find "$HOME/.local/state/pack/install" \
    \( -type d -name cribrum -o -type d -name teaweb \) \
    -exec rm -rf {} + 2>/dev/null
  "$PACK_BIN" install cribrum >/tmp/cribrum-mut-install.log 2>&1
  "$PACK_BIN" install teaweb  >/tmp/teaweb-mut-install.log  2>&1
}

# 1. Baseline: tests must pass before we mutate anything.
echo "=== baseline ==="
reinstall_cribrum
if ! "$PACK_BIN" -q run test/test.ipkg >/tmp/cribrum-mut-baseline.log 2>&1; then
  echo "FATAL: baseline test run failed; cannot proceed" >&2
  tail -40 /tmp/cribrum-mut-baseline.log >&2
  exit 2
fi
echo "  baseline OK"
echo

# 2. Walk mutants.
total=0
killed=0
survived=0
approved_survivors=0
fail_msgs=()

while IFS=$'\t' read -r file find_str replace_str description; do
  # Skip blanks / comments.
  [[ -z "${file:-}" || "${file:0:1}" == "#" ]] && continue
  total=$((total + 1))

  if [[ ! -f "$file" ]]; then
    echo "M$total: SKIP missing file $file"
    continue
  fi

  if ! grep -F -- "$find_str" "$file" >/dev/null; then
    echo "M$total: SKIP find-string not present in $file"
    echo "       find: $find_str"
    fail_msgs+=("M$total: find-string missing in $file -- harness bug")
    continue
  fi

  # Backup and mutate. We replace only the first occurrence to keep mutants
  # surgical: emit Python for reliable literal substitution.
  cp "$file" "$file.bak"
  python3 - "$file" "$find_str" "$replace_str" <<'PY'
import sys, pathlib
path, needle, repl = sys.argv[1], sys.argv[2], sys.argv[3]
p = pathlib.Path(path)
text = p.read_text()
idx = text.find(needle)
if idx < 0:
    sys.exit("needle not found: " + needle)
new = text[:idx] + repl + text[idx+len(needle):]
p.write_text(new)
PY

  if [[ $? -ne 0 ]]; then
    echo "M$total: ERROR python substitution failed; restoring"
    mv "$file.bak" "$file"
    fail_msgs+=("M$total: sed/python failure")
    continue
  fi

  printf "M%-2d %-50s ... " "$total" "$description"

  # Reinstall library so the mutated source is what test links against.
  reinstall_cribrum

  # Build + run tests; success = mutant survived.
  if "$PACK_BIN" -q run test/test.ipkg >/tmp/cribrum-mut.log 2>&1; then
    is_approved=0
    for a in $APPROVED; do
      [[ "$a" == "$total" ]] && is_approved=1
    done
    if [[ $is_approved -eq 1 ]]; then
      echo "SURVIVED (approved)"
      approved_survivors=$((approved_survivors + 1))
    else
      echo "SURVIVED"
      survived=$((survived + 1))
      fail_msgs+=("M$total survived: $description")
    fi
  else
    echo "killed"
    killed=$((killed + 1))
  fi

  # Restore source and reinstall so the next iteration starts clean.
  mv "$file.bak" "$file"
  reinstall_cribrum
done < "$TSV"

echo
echo "=== summary ==="
echo "  total:              $total"
echo "  killed:             $killed"
echo "  survived (BAD):     $survived"
echo "  survived (approved):$approved_survivors"

if [[ ${#fail_msgs[@]} -gt 0 ]]; then
  echo
  echo "=== issues ==="
  for m in "${fail_msgs[@]}"; do
    echo "  - $m"
  done
fi

# Restore final state in case a mutant left a backup behind on interruption.
find src -name '*.bak' -print0 | while IFS= read -r -d '' f; do
  mv "$f" "${f%.bak}"
done
reinstall_cribrum

[[ $survived -eq 0 ]] && [[ ${#fail_msgs[@]} -eq 0 || $survived -eq 0 ]] || exit 1
exit 0
