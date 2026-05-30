// Cribrum AA-catalog ingestion — emits src/Cribrum/AA/Catalog/Generated.idr
// from `ingest/aa.ts`.
//
// `aa.ts` is the typed staging area for the W3C ACT-rules pull (plan.dj
// §P3.1). For now it's hand-curated; the script's I/O shape is the same
// either way — emit a deterministic Idris module, drift-gate it.
//
// Run:
//   cd ingest && npm install && npm run ingest:aa        # regenerate
//   cd ingest && npm install && npm run ingest:aa:check  # gate (no write)

import { writeFileSync, readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { AA_CATALOG, AARuleRow, Confidence, Severity } from "./aa.js";
import { loadActRows } from "./act-rules.js";

// The full catalog = hand-curated Cribrum rows (aa.ts) + the upstream
// ACT-rules pull (act-rules.ts, vendored corpus). Both share the AARuleRow
// shape and merge into one generated Idris module. ACT rows carry provenance
// + Applicability/Expectation data; Cribrum rows default those fields.
function loadCatalog(): AARuleRow[] {
  return [...AA_CATALOG, ...loadActRows()];
}

// -----------------------------------------------------------------------------
// Invariant checks (run pre-emit).
// -----------------------------------------------------------------------------

// Idris-side per-rule constant names. Default = camelCase of kebab id with a
// `rule` prefix. Overrides preserve the existing Cribrum.AA.Catalog names
// (so `Cribrum.AA.Pass`'s call sites stay unchanged through the refactor).
const IDRIS_NAME_OVERRIDES: Record<string, string> = {
  "label-for-control": "ruleLabelFor",
};

function defaultIdrisName(id: string): string {
  const camel = id.split("-").map((part, i) =>
    i === 0 ? part : part.charAt(0).toUpperCase() + part.slice(1)
  ).join("");
  return "rule" + camel.charAt(0).toUpperCase() + camel.slice(1);
}

function idrisNameFor(id: string): string {
  return IDRIS_NAME_OVERRIDES[id] ?? defaultIdrisName(id);
}

function checkInvariants(rows: AARuleRow[]): void {
  // I1: id uniqueness within aa.ts.
  const seenIds = new Set<string>();
  for (const r of rows) {
    if (seenIds.has(r.id)) {
      throw new Error(`I1 violated: duplicate rule id "${r.id}" in aa.ts`);
    }
    seenIds.add(r.id);
  }

  // I2: Idris-name uniqueness (after override resolution).
  const seenNames = new Set<string>();
  for (const r of rows) {
    const n = idrisNameFor(r.id);
    if (seenNames.has(n)) {
      throw new Error(
        `I2 violated: rule "${r.id}" maps to Idris name "${n}" which collides with another rule`);
    }
    seenNames.add(n);
  }

  // I3: WCAG SC must be dotted digits ("1.1.1", "2.4.4", ...).
  const scShape = /^[0-9]+(\.[0-9]+)+$/;
  for (const r of rows) {
    if (!scShape.test(r.wcag)) {
      throw new Error(`I3 violated: rule "${r.id}" has malformed wcag SC "${r.wcag}"`);
    }
  }

  // I4: level ∈ {A, AA}.
  for (const r of rows) {
    if (r.level !== "A" && r.level !== "AA") {
      throw new Error(`I4 violated: rule "${r.id}" has unknown level "${r.level}"`);
    }
  }

  // I5: Structural rules with Warning/Info severity are flagged as a
  // warning — the partitioning audit (plan §P3.3) expects Structural
  // findings to escalate to Error by default. Structural means the
  // contract is enforced by construction (a typed promotion in
  // `Cribrum.AA.Typed` + a hard `Left` in `Cribrum.Elaborate`), so a
  // softer Warning/Info severity is inconsistent with how the rule
  // actually fails. Listed here for visibility, not blocked.
  for (const r of rows) {
    if (r.confidence === "Structural" && r.severity !== "Error") {
      console.warn(
        `I5 notice: structural rule "${r.id}" has severity "${r.severity}" (expected Error)`);
    }
  }
}

// -----------------------------------------------------------------------------
// Emit Idris source.
// -----------------------------------------------------------------------------

function emitConfidence(c: Confidence): string {
  // Idris Confidence ctors share names with the TS string literals.
  return c;
}

function emitSeverity(s: Severity): string {
  return s;
}

function escapeIdrisString(s: string): string {
  // JSON.stringify gives us \" and \\ escapes that Idris accepts as-is
  // for ordinary string literals.
  return JSON.stringify(s);
}

function emitSource(s: AARuleRow["source"]): string {
  // Idris RuleSource ctors are Cribrum | Act.
  return s === "act" ? "Act" : "Cribrum";
}

function emitStringList(xs: string[] | undefined): string {
  if (!xs || xs.length === 0) return "[]";
  return "[" + xs.map(escapeIdrisString).join(", ") + "]";
}

function emitRule(r: AARuleRow): string {
  const lines = [
    `${idrisNameFor(r.id)} : Rule`,
    `${idrisNameFor(r.id)} = MkRule`,
    `  { id            = ${escapeIdrisString(r.id)}`,
    `  , wcag          = ${escapeIdrisString(r.wcag)}`,
    `  , level         = ${escapeIdrisString(r.level)}`,
    `  , title         = ${escapeIdrisString(r.title)}`,
    `  , confidence    = ${emitConfidence(r.confidence)}`,
    `  , severity      = ${emitSeverity(r.severity)}`,
    `  , source        = ${emitSource(r.source)}`,
    `  , actId         = ${escapeIdrisString(r.actId ?? "")}`,
    `  , ruleType      = ${escapeIdrisString(r.ruleType ?? "")}`,
    `  , wcagAll       = ${emitStringList(r.wcagAll)}`,
    `  , applicability = ${escapeIdrisString(r.applicability ?? "")}`,
    `  , expectation   = ${escapeIdrisString(r.expectation ?? "")}`,
    `  }`,
  ];
  return "public export\n" + lines.join("\n");
}

function emitModule(
  catalog: AARuleRow[],
  aaSourceSha: string,
  catalogSha: string,
): string {
  // Sort lexicographically by id (determinism). Same order in both the
  // per-rule definitions and the `generatedRules` list.
  const sorted = [...catalog].sort((a, b) => a.id.localeCompare(b.id));
  const ruleBlocks = sorted.map(emitRule).join("\n\n");
  const listLines  = sorted.map((r, i) =>
    (i === 0 ? "  [ " : "  , ") + idrisNameFor(r.id)).join("\n");

  return `||| AUTO-GENERATED by ingest/aa-catalog.ts — do not edit by hand.
||| aa.ts: sha256 ${aaSourceSha}
||| catalog hash: sha256 ${catalogSha}
module Cribrum.AA.Catalog.Generated

import Cribrum.AA.Catalog.Types

%default total

--------------------------------------------------------------------------------
-- Per-rule constants.
--------------------------------------------------------------------------------

${ruleBlocks}

--------------------------------------------------------------------------------
-- All rules in the catalog, lexicographically sorted by id.
--------------------------------------------------------------------------------

public export
generatedRules : List Rule
generatedRules =
${listLines}
  ]
`;
}

function sha256(s: string): string {
  return createHash("sha256").update(s, "utf8").digest("hex");
}

// -----------------------------------------------------------------------------
// Main.
// -----------------------------------------------------------------------------

async function main() {
  const check = process.argv.includes("--check");
  const outputPath = resolve(import.meta.dirname, "../src/Cribrum/AA/Catalog/Generated.idr");
  const aaSourcePath = resolve(import.meta.dirname, "aa.ts");

  // Merge hand-curated (aa.ts) + ingested ACT-rules rows.
  const catalog = loadCatalog();
  checkInvariants(catalog);

  const aaSource = readFileSync(aaSourcePath, "utf8");
  const aaSourceSha = sha256(aaSource);
  // Catalog hash is over the canonical JSON serialisation of the sorted
  // *merged* catalog — independent of source-file formatting, and covering
  // the ACT corpus content (via its parsed rows) as well as aa.ts.
  const sortedCatalog = [...catalog].sort((a, b) => a.id.localeCompare(b.id));
  const catalogSha = sha256(JSON.stringify(sortedCatalog));

  const generated = emitModule(catalog, aaSourceSha, catalogSha);

  if (check) {
    if (!existsSync(outputPath)) {
      console.error(`error: ${outputPath} does not exist; run \`npm run ingest:aa\` to create.`);
      process.exit(1);
    }
    const existing = readFileSync(outputPath, "utf8");
    if (existing !== generated) {
      const embeddedAaSha = /aa\.ts: sha256 ([0-9a-f]+)/.exec(existing)?.[1];
      const embeddedCatSha = /catalog hash: sha256 ([0-9a-f]+)/.exec(existing)?.[1];
      console.error("error: generated AA catalog differs from checked-in copy.");
      if (embeddedAaSha && embeddedAaSha !== aaSourceSha) {
        console.error(`  aa.ts drift: ${embeddedAaSha} vs ${aaSourceSha}`);
      }
      if (embeddedCatSha && embeddedCatSha !== catalogSha) {
        console.error(`  catalog hash drift: ${embeddedCatSha} vs ${catalogSha}`);
      }
      console.error("  Run `npm run ingest:aa` to refresh.");
      process.exit(1);
    }
    console.log(`AA catalog round-trip OK (${catalog.length} rule(s))`);
  } else {
    writeFileSync(outputPath, generated, "utf8");
    console.log(`wrote ${outputPath} (${catalog.length} rule(s))`);
  }
}

main().catch(e => { console.error(e); process.exit(2); });
