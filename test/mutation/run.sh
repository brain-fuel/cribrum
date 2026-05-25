#!/usr/bin/env bash
#
# Mutation-testing harness for Cribrum + TEAWeb.
#
# Reads test/mutation/mutants.tsv (TAB-separated):
#   FILE \t FIND \t REPLACE \t DESCRIPTION
#
# For each mutant: backup -> sed substitute -> rebuild + run downstream
# tests -> restore. A surviving mutant = tests still pass (exit 0).
# The script exits non-zero if any non-approved mutant survives.
#
# === Scope control (changed-file mode) ===
#
# By default the gate restricts itself to mutants whose FILE has changed
# vs. the git baseline. If no source files changed, the gate is a no-op
# (exit 0). Override the baseline with MUTATION_BASE:
#
#   MUTATION_BASE=HEAD       ./run.sh   # default: worktree vs last commit
#   MUTATION_BASE=origin/main ./run.sh  # full PR scope
#   MUTATION_BASE=ALL        ./run.sh   # disable filtering, run everything
#
# Changed files include both committed changes since the base AND
# uncommitted working-tree edits.
#
# Per mutated file, only downstream test suites are run:
#   src/Cribrum/* → cribrum tests + teaweb tests
#   src/TEAWeb/*  → teaweb tests only
#
# === Approved survivors ===
#
#   APPROVED="3 7"   ./run.sh
#
# Listed (1-based) mutants are still executed but their survival does
# NOT fail the run (semantically-equivalent mutants).

set -u
set -o pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TSV="$ROOT/test/mutation/mutants.tsv"
PACK_BIN="${PACK_BIN:-pack}"
APPROVED="${APPROVED:-}"
MUTATION_BASE="${MUTATION_BASE:-HEAD}"

cd "$ROOT"

# Trap signals + normal exit: any `.bak` left behind by a killed mutant
# iteration gets restored. Without this, ^C / SIGTERM mid-iteration
# leaves the source tree mutated and subsequent builds see corrupt code
# until someone manually `mv`s the .bak back.
cleanup_baks () {
  find src -name '*.bak' 2>/dev/null | while IFS= read -r f; do
    mv "$f" "${f%.bak}"
  done
}
trap cleanup_baks EXIT INT TERM

if [[ ! -r "$TSV" ]]; then
  echo "FATAL: cannot read $TSV" >&2
  exit 2
fi

#-----------------------------------------------------------------------
# Changed-file detection.
#-----------------------------------------------------------------------

# Compute the set of files we care about. "ALL" disables the filter
# (gate behaves like the unscoped pre-baseline harness).
declare -A CHANGED_SET=()
USE_FILTER=1
if [[ "$MUTATION_BASE" == "ALL" ]]; then
  USE_FILTER=0
  echo "=== scope: ALL (filter disabled) ==="
else
  # Committed changes vs the baseline ref + uncommitted working-tree
  # edits. Restrict to src/ — examples/, docs/, tests/ are out of scope
  # for the library mutation gate.
  while IFS= read -r f; do
    [[ -n "$f" ]] && CHANGED_SET["$f"]=1
  done < <(
    {
      git diff --name-only "$MUTATION_BASE" -- 'src/' 2>/dev/null
      git diff --name-only -- 'src/' 2>/dev/null
      git ls-files --others --exclude-standard -- 'src/' 2>/dev/null
    } | sort -u
  )
  echo "=== scope: src/ files changed vs $MUTATION_BASE ==="
  if [[ ${#CHANGED_SET[@]} -eq 0 ]]; then
    echo "  (none — gate is a no-op)"
    exit 0
  else
    for f in "${!CHANGED_SET[@]}"; do
      echo "  $f"
    done
  fi
fi
echo

#-----------------------------------------------------------------------
# Per-file downstream test-suite mapping.
#
# Given a mutated source file, emit the test ipkgs that must pass for
# the mutant to count as killed. Cribrum changes can break TEAWeb (it
# imports cribrum), so they require both suites; TEAWeb-only changes
# need only the teaweb suite.
#-----------------------------------------------------------------------

test_suites_for () {
  local file="$1"
  case "$file" in
    src/Cribrum/*) echo "test/test.ipkg test/teaweb/teaweb-test.ipkg" ;;
    src/TEAWeb/*)  echo "test/teaweb/teaweb-test.ipkg" ;;
    *)             echo "test/test.ipkg test/teaweb/teaweb-test.ipkg" ;;
  esac
}

# Run a suite under pack -q; returns the suite's exit code.
run_suite () {
  local suite="$1"
  "$PACK_BIN" -q run "$suite" >/tmp/cribrum-mut.log 2>&1
}

#-----------------------------------------------------------------------
# Pack install plumbing. Wipes cribrum + teaweb installs so each
# iteration links against fresh source.
#-----------------------------------------------------------------------
reinstall_libs () {
  rm -rf build test/build test/teaweb/build
  find "$HOME/.local/state/pack/install" \
    \( -type d -name cribrum -o -type d -name teaweb \) \
    -exec rm -rf {} + 2>/dev/null
  "$PACK_BIN" install cribrum >/tmp/cribrum-mut-install.log 2>&1
  "$PACK_BIN" install teaweb  >/tmp/teaweb-mut-install.log  2>&1
}

#-----------------------------------------------------------------------
# Baseline: tests must pass before we mutate anything.
#-----------------------------------------------------------------------
echo "=== baseline ==="
reinstall_libs

# Baseline runs every distinct suite that any in-scope mutant might
# trigger (a superset of the per-mutant suite-set for the scoped run).
declare -A BASELINE_SUITES=()
while IFS=$'\t' read -r file _ _ _; do
  [[ -z "${file:-}" || "${file:0:1}" == "#" ]] && continue
  [[ $USE_FILTER -eq 1 && -z "${CHANGED_SET[$file]:-}" ]] && continue
  for s in $(test_suites_for "$file"); do
    BASELINE_SUITES["$s"]=1
  done
done < "$TSV"

if [[ ${#BASELINE_SUITES[@]} -eq 0 ]]; then
  echo "  (no in-scope mutants reference changed files — gate is a no-op)"
  exit 0
fi

for suite in "${!BASELINE_SUITES[@]}"; do
  printf "  %-40s ... " "$suite"
  if run_suite "$suite"; then
    echo "OK"
  else
    echo "FAIL"
    echo "FATAL: baseline test run failed; cannot proceed" >&2
    tail -40 /tmp/cribrum-mut.log >&2
    exit 2
  fi
done
echo

#-----------------------------------------------------------------------
# Walk mutants.
#-----------------------------------------------------------------------
total=0
killed=0
skipped_scope=0
survived=0
approved_survivors=0
fail_msgs=()

mutant_index=0
while IFS=$'\t' read -r file find_str replace_str description; do
  # Skip blanks / comments.
  [[ -z "${file:-}" || "${file:0:1}" == "#" ]] && continue
  mutant_index=$((mutant_index + 1))

  # Out-of-scope mutants are silently skipped; they're counted only in
  # the "skipped" summary line.
  if [[ $USE_FILTER -eq 1 && -z "${CHANGED_SET[$file]:-}" ]]; then
    skipped_scope=$((skipped_scope + 1))
    continue
  fi

  total=$((total + 1))

  if [[ ! -f "$file" ]]; then
    echo "M$mutant_index: SKIP missing file $file"
    continue
  fi

  if ! grep -F -- "$find_str" "$file" >/dev/null; then
    echo "M$mutant_index: SKIP find-string not present in $file"
    echo "       find: $find_str"
    fail_msgs+=("M$mutant_index: find-string missing in $file -- harness bug")
    continue
  fi

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
    echo "M$mutant_index: ERROR python substitution failed; restoring"
    mv "$file.bak" "$file"
    fail_msgs+=("M$mutant_index: sed/python failure")
    continue
  fi

  printf "M%-2d %-50s ... " "$mutant_index" "$description"

  reinstall_libs

  # Run each downstream suite for the mutated file. The mutant "survives"
  # only if every suite passes; the first failing suite kills the mutant.
  suites=$(test_suites_for "$file")
  mutant_killed=0
  for suite in $suites; do
    if ! run_suite "$suite"; then
      mutant_killed=1
      break
    fi
  done

  if [[ $mutant_killed -eq 1 ]]; then
    echo "killed"
    killed=$((killed + 1))
  else
    is_approved=0
    for a in $APPROVED; do
      [[ "$a" == "$mutant_index" ]] && is_approved=1
    done
    if [[ $is_approved -eq 1 ]]; then
      echo "SURVIVED (approved)"
      approved_survivors=$((approved_survivors + 1))
    else
      echo "SURVIVED"
      survived=$((survived + 1))
      fail_msgs+=("M$mutant_index survived: $description")
    fi
  fi

  mv "$file.bak" "$file"
  reinstall_libs
done < "$TSV"

echo
echo "=== summary ==="
echo "  scope:              $([[ $USE_FILTER -eq 1 ]] && echo "changed-files vs $MUTATION_BASE" || echo ALL)"
echo "  run:                $total"
echo "  killed:             $killed"
echo "  survived (BAD):     $survived"
echo "  survived (approved):$approved_survivors"
[[ $USE_FILTER -eq 1 ]] && echo "  skipped (scope):    $skipped_scope"

if [[ ${#fail_msgs[@]} -gt 0 ]]; then
  echo
  echo "=== issues ==="
  for m in "${fail_msgs[@]}"; do
    echo "  - $m"
  done
fi

# Final-state restoration is handled by the EXIT trap above; reinstall
# the libs so the post-gate environment matches what `pack install
# cribrum/teaweb` would produce.
reinstall_libs

[[ $survived -eq 0 ]] || exit 1
exit 0
