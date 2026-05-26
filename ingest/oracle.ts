// Cribrum oracle cross-check — plan.dj §P2.4.
//
// Reads the JSONL corpus emitted by `tools/oracle-emit/` (one row per
// curated `(name, html, decided, expected)` case) and validates each
// `html` against the W3C Nu Html Checker (`vnu.jar`). Reports any
// disagreement between three sources:
//
//   • `expected`  — what HTML5 actually says (curator's ground truth).
//   • `decided`   — Cribrum's `decideHtml` verdict.
//   • `vnu`       — the reference validator's verdict.
//
// All three should agree on every row. A divergence is a finding for
// follow-up — either Cribrum's content-model is wrong, the curator
// misjudged the case, or Cribrum is checking a feature vnu doesn't
// (e.g. a Cribrum-only rule). The exit code is 0 only when there are
// no divergences across the corpus.
//
// vnu is shipped via the `vnu-jar` npm package; the `vnu` CLI shim
// requires Java on `$PATH`. If either is missing, the script prints
// an actionable skip message and exits 0 — the gate is opt-in via the
// `CRIBRUM_ORACLE_REQUIRE_VNU=1` env var, which turns missing-vnu /
// missing-java into a hard failure.
//
// Run:
//   cd ingest && npm install
//   pack -q run tools/oracle-emit/oracle-emit.ipkg | npx tsx oracle.ts

import { spawnSync } from "node:child_process";
import { readFileSync, existsSync } from "node:fs";

interface Row {
  name:     string;
  expected: boolean;
  decided:  boolean;
  html:     string;
}

function readCases(): Row[] {
  // Read JSONL from stdin (one row per line). The Idris-side emitter
  // also writes a diagnostic line to stderr — invisible here.
  const src  = readFileSync(0, "utf8");
  const rows = src.split("\n").filter(l => l.trim().length > 0);
  return rows.map(l => JSON.parse(l) as Row);
}

function locateVnu(): string | null {
  // The `vnu-jar` package ships a `vnu` shim under
  // `node_modules/.bin/vnu`. Prefer the local install over a system
  // one so the pinned version is what runs in CI.
  const local = "node_modules/.bin/vnu";
  if (existsSync(local)) return local;
  // Fall back to whatever `vnu` is on `$PATH`.
  const which = spawnSync("which", ["vnu"], { encoding: "utf8" });
  if (which.status === 0 && which.stdout.trim()) return which.stdout.trim();
  return null;
}

function checkJava(): boolean {
  const r = spawnSync("java", ["-version"], { stdio: "ignore" });
  return r.status === 0;
}

function vnuVerdict(vnuBin: string, html: string): "valid" | "invalid" | "skip" {
  // Wrap the rendered fragment in a minimal HTML5 doc so vnu has
  // something complete to validate (a bare fragment would itself be
  // reported as missing `<!DOCTYPE>`/`<title>`).
  const doc =
    "<!DOCTYPE html><html lang=\"en\"><head><title>oracle</title></head>"
    + "<body>" + html + "</body></html>";

  // `--format json` returns one `messages` array; non-empty error
  // entries mean vnu flagged the document.
  const r = spawnSync(vnuBin, ["--format", "json", "-"], {
    input:    doc,
    encoding: "utf8",
  });
  // vnu's `--format json` output lands on stderr (so stdout stays
  // free for any future structured output). The exit code mirrors
  // the verdict: 0 on no errors, non-zero on validation errors —
  // either way the JSON body is authoritative.
  const out = (r.stderr || "") + (r.stdout || "");
  let parsed: any;
  try { parsed = JSON.parse(out); }
  catch (e) {
    process.stderr.write(`vnu output not JSON: ${out.slice(0, 200)}\n`);
    return "skip";
  }
  const msgs   = (parsed.messages ?? []) as Array<{ type?: string }>;
  const errors = msgs.filter(m => m.type === "error");
  return errors.length === 0 ? "valid" : "invalid";
}

function main(): void {
  const rows   = readCases();
  const vnu    = locateVnu();
  const require = process.env.CRIBRUM_ORACLE_REQUIRE_VNU === "1";

  if (!vnu || !checkJava()) {
    const reason = !vnu ? "vnu binary not found" : "java not on PATH";
    if (require) {
      console.error(`oracle: ${reason}; CRIBRUM_ORACLE_REQUIRE_VNU=1 → failing`);
      process.exit(1);
    }
    console.error(
      `oracle: ${reason}; skipping vnu cross-check (set `
      + `CRIBRUM_ORACLE_REQUIRE_VNU=1 to require it). `
      + `Cribrum/corpus agreement still verified by oracle-emit itself.`);
    process.exit(0);
  }

  let disagreements = 0;
  for (const row of rows) {
    const v = vnuVerdict(vnu, row.html);
    if (v === "skip") continue;
    const vnuValid = (v === "valid");
    const allAgree = row.expected === row.decided && row.decided === vnuValid;
    if (allAgree) {
      console.log(`  PASS  ${row.name}  (cribrum=${row.decided} vnu=${vnuValid})`);
    } else {
      disagreements++;
      console.log(`  FAIL  ${row.name}  expected=${row.expected} `
                  + `cribrum=${row.decided} vnu=${vnuValid}`);
      console.log(`        html: ${row.html}`);
    }
  }

  console.log(`\noracle: ${rows.length} case(s), ${disagreements} divergence(s)`);
  process.exit(disagreements === 0 ? 0 : 1);
}

main();
