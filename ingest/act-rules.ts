// ACT-rules upstream ingestion — parses the W3C-CG ACT rule front-matter
// (vendored under `ingest/act-rules/_rules/*.md`) into the same `AARuleRow`
// shape that `ingest/aa.ts` hand-curates, so the two sources merge into one
// generated catalog (`Cribrum.AA.Catalog.Generated`).
//
// Plan.dj §P3.1 targets ACT-rule ingestion. The decision here is
// *infrastructure before content*: wire the pull so future rule growth is
// data-entry (drop a rule `.md` under `act-rules/_rules/`, rerun ingest),
// not hand-written Idris.
//
// UPSTREAM SOURCE
// ---------------
// github.com/act-rules/act-rules.github.io — the W3C-CG ACT rules. Each
// rule is a Markdown file under `_rules/<name>-<id>.md` whose YAML front-
// matter carries:
//   - id          : 6-char stable rule id (e.g. "97a4e1")
//   - name        : human-readable title
//   - rule_type   : "atomic" | "composite"
//   - accessibility_requirements : map of "<spec>:<key>" -> conformance
//       flags. WCAG success criteria appear as keys like "wcag20:4.1.2"
//       / "wcag21:1.4.11" with a trailing comment naming the SC and (in
//       parentheses) its conformance level, e.g. "# Name, Role, Value (A)".
// Two prose sections follow the front-matter and are carried as data:
//   - ## Applicability — which nodes the rule applies to.
//   - ## Expectation   — the pass condition.
//
// VENDORING (offline-reproducible, mirrors djot-ref.ts)
// -----------------------------------------------------
// The rule `.md` files are vendored in-repo at a pinned commit rather than
// fetched at ingest time, so `make ingest` / `make ingest-check` run with
// no network. Refreshing the corpus is a deliberate content change: bump
// `PINNED_COMMIT`, re-download into `act-rules/_rules/`, rerun ingest, and
// review the catalog diff in PR. To add rules, drop their `.md` files in
// the same directory (or run the `download` helper below) and rerun.
//
// Run:
//   cd ingest && npm install && npm run ingest:act:download   # refresh corpus
//   cd ingest && npm run ingest                               # regenerate catalog
//   cd ingest && npm run ingest:check                         # gate

import { readFileSync, readdirSync, existsSync, writeFileSync } from "node:fs";
import { resolve, join } from "node:path";
import type { AARuleRow, Confidence, Severity } from "./aa.js";

// -----------------------------------------------------------------------------
// Pinned upstream commit. Bump deliberately; drift between this constant and
// the vendored `_rules/*.md` is the only knob.
// -----------------------------------------------------------------------------
export const PINNED_COMMIT =
  "5812a1afd8f4eca3a9dd40ec56675191b098469d";
export const SOURCE_REPO = "act-rules/act-rules.github.io";

const RULES_DIR = resolve(import.meta.dirname, "act-rules", "_rules");

// -----------------------------------------------------------------------------
// Front-matter extraction. ACT files open with a `---` fenced YAML block.
// We hand-parse the handful of fields we need rather than pull a YAML dep
// (matching djot-ref.ts's hand-rolled approach). The grammar we rely on is
// narrow and stable across the corpus.
// -----------------------------------------------------------------------------

interface ParsedRequirement {
  // The dotted WCAG SC, e.g. "4.1.2".
  sc: string;
  // Conformance level parsed from the trailing comment, e.g. "A" | "AA".
  level: string;
  // Whether this requirement counts forConformance (vs. a related technique).
  forConformance: boolean;
}

export interface ParsedActRule {
  actId: string; // 6-char ACT id
  name: string;
  ruleType: string; // "atomic" | "composite" | ...
  requirements: ParsedRequirement[];
  applicability: string;
  expectation: string;
  sourceFile: string;
}

function splitFrontMatter(src: string): { yaml: string; body: string } {
  // Front-matter is the first `---`...`---` block at the top of the file.
  const m = /^---\r?\n([\s\S]*?)\r?\n---\r?\n([\s\S]*)$/.exec(src);
  if (!m) throw new Error("no YAML front-matter found");
  return { yaml: m[1], body: m[2] };
}

// Parse `accessibility_requirements:` — a nested map whose immediate child
// keys are "<spec>:<criterion>" with an optional trailing "# … (LEVEL)"
// comment. We keep only WCAG success-criteria keys (spec starts with
// "wcag" and the criterion is dotted digits).
function parseRequirements(yaml: string): ParsedRequirement[] {
  const lines = yaml.split(/\r?\n/);
  const out: ParsedRequirement[] = [];

  // Find the `accessibility_requirements:` block and its child indentation.
  let i = lines.findIndex((l) => /^accessibility_requirements:\s*$/.test(l));
  if (i < 0) return out;
  i += 1;

  // Child keys are indented (two spaces in the corpus); a top-level key
  // (no leading space, ends in `:`) terminates the block.
  const childKey = /^(\s+)([A-Za-z][\w-]*:[^\s#]+):\s*(#.*)?$/;
  let blockIndent: number | null = null;

  for (; i < lines.length; i++) {
    const line = lines[i];
    if (line.trim() === "") continue;
    const indent = line.length - line.trimStart().length;
    if (indent === 0) break; // dedent to a new top-level key -> block done

    const km = childKey.exec(line);
    if (!km) continue;
    if (blockIndent === null) blockIndent = km[1].length;
    if (km[1].length !== blockIndent) continue; // only direct children

    const fullKey = km[2]; // e.g. "wcag20:4.1.2" or "wcag-technique:G94"
    const comment = km[3] ?? ""; // e.g. "# Name, Role, Value (A)"
    const colon = fullKey.indexOf(":");
    const spec = fullKey.slice(0, colon);
    const criterion = fullKey.slice(colon + 1);

    // Keep only WCAG success criteria (dotted digits); drop techniques (G94…).
    if (!/^wcag\d/.test(spec)) continue;
    if (!/^\d+(\.\d+)+$/.test(criterion)) continue;

    // Level from the trailing comment's last parenthesised token.
    const lvl = /\(([A]{1,3})\)\s*$/.exec(comment.replace(/^#\s*/, ""));
    const level = lvl ? lvl[1] : "";

    // forConformance defaults to scanning the nested body for
    // `forConformance: true`. We look ahead at deeper-indented lines.
    let forConformance = false;
    for (let j = i + 1; j < lines.length; j++) {
      const sub = lines[j];
      if (sub.trim() === "") continue;
      const subIndent = sub.length - sub.trimStart().length;
      if (subIndent <= blockIndent) break; // back to a sibling/parent
      if (/forConformance:\s*true/.test(sub)) forConformance = true;
    }

    out.push({ sc: criterion, level, forConformance });
  }

  return out;
}

function parseScalar(yaml: string, key: string): string | undefined {
  // Matches `key: value` (single-line) or `key: |` block scalars; for our
  // needs (id, name, rule_type) the values are single-line.
  const re = new RegExp(`^${key}:\\s*(.+?)\\s*$`, "m");
  const m = re.exec(yaml);
  if (!m) return undefined;
  let v = m[1].trim();
  // Strip surrounding quotes if present.
  if (
    (v.startsWith('"') && v.endsWith('"')) ||
    (v.startsWith("'") && v.endsWith("'"))
  ) {
    v = v.slice(1, -1);
  }
  return v;
}

// Extract a `## Section` body up to (but not including) the next `## `
// heading. Prose is flattened to a single line (links/markdown stripped to
// readable text) so it round-trips cleanly into an Idris string literal.
function extractSection(body: string, heading: string): string {
  const lines = body.split(/\r?\n/);
  const start = lines.findIndex(
    (l) => l.trim().toLowerCase() === `## ${heading}`.toLowerCase(),
  );
  if (start < 0) return "";
  const out: string[] = [];
  for (let i = start + 1; i < lines.length; i++) {
    if (/^##\s/.test(lines[i])) break;
    out.push(lines[i]);
  }
  return flattenProse(out.join("\n"));
}

// Collapse Markdown prose to a single readable line:
//  - [text](url) -> text
//  - **/* emphasis and ` code fences -> bare text
//  - newlines / list markers -> single spaces
function flattenProse(s: string): string {
  return s
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1") // links
    .replace(/\[([^\]]+)\]\[[^\]]*\]/g, "$1") // reference links
    .replace(/[`*_]/g, "") // code/emphasis marks
    .replace(/^\s*[-*]\s+/gm, "") // list bullets
    .replace(/\s*\n\s*/g, " ") // newlines -> space
    .replace(/\s{2,}/g, " ")
    .trim();
}

export function parseActRuleFile(path: string, src: string): ParsedActRule {
  const { yaml, body } = splitFrontMatter(src);
  const actId = parseScalar(yaml, "id");
  const name = parseScalar(yaml, "name");
  const ruleType = parseScalar(yaml, "rule_type") ?? "atomic";
  if (!actId) throw new Error(`${path}: missing front-matter id`);
  if (!name) throw new Error(`${path}: missing front-matter name`);
  return {
    actId,
    name,
    ruleType,
    requirements: parseRequirements(yaml),
    applicability: extractSection(body, "Applicability"),
    expectation: extractSection(body, "Expectation"),
    sourceFile: path.split("/").pop() ?? path,
  };
}

// -----------------------------------------------------------------------------
// Mapping ACT -> Cribrum `AARuleRow`.
// -----------------------------------------------------------------------------

// Stable Cribrum rule-id slug for an ACT rule: kebab name + short ACT id, so
// the slug is human-meaningful AND collision-free against the hand-curated
// aa.ts ids (which use plain kebab slugs). The ACT id suffix guarantees
// uniqueness across the corpus.
function slugFor(name: string, actId: string): string {
  const kebab = name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  return `act-${kebab}-${actId}`;
}

// Pick the catalog's single primary WCAG SC + level. We prefer the
// lowest-numbered forConformance WCAG SC (deterministic, and the lowest SC
// is invariably the core requirement; AAA criteria sort last).
function primaryRequirement(
  reqs: ParsedRequirement[],
): ParsedRequirement | undefined {
  const conf = reqs.filter((r) => r.forConformance);
  const pool = conf.length > 0 ? conf : reqs;
  if (pool.length === 0) return undefined;
  return [...pool].sort((a, b) => cmpSc(a.sc, b.sc))[0];
}

function cmpSc(a: string, b: string): number {
  const pa = a.split(".").map(Number);
  const pb = b.split(".").map(Number);
  for (let i = 0; i < Math.max(pa.length, pb.length); i++) {
    const d = (pa[i] ?? 0) - (pb[i] ?? 0);
    if (d !== 0) return d;
  }
  return 0;
}

// Truncate prose carried into the generated module — full prose is long and
// the Idris side only needs a stable descriptive snapshot for now. Keep
// generous headroom (the fields are data, not display strings).
function clamp(s: string, max = 600): string {
  if (s.length <= max) return s;
  return s.slice(0, max - 1).trimEnd() + "…";
}

export function actRuleToRow(rule: ParsedActRule): AARuleRow {
  const primary = primaryRequirement(rule.requirements);
  const wcag = primary?.sc ?? "";
  // Conformance level: AA if the chosen SC is AA-level, A if A-level. The
  // catalog `level` field is "A" | "AA"; we map AAA-only rules to "AA" as
  // the closest representable conformance bucket (none of the proof rules
  // here are AAA-only, but be defensive).
  const lvl = primary?.level ?? "A";
  const level: "A" | "AA" = lvl === "A" ? "A" : "AA";

  // Classification: ACT expectations are accessible-name / accessibility-
  // tree predicates that need computed a11y-tree state, so they are NOT yet
  // statically type-expressible on Cribrum's HTML tree alone. They land as
  // Heuristic (pass-only, never claims conformance) until a follow-up
  // promotes those with a tree-decidable expectation to Structural.
  const confidence: Confidence = "Heuristic";
  const severity: Severity = "Warning";

  return {
    id: slugFor(rule.name, rule.actId),
    wcag,
    level,
    title: rule.name,
    confidence,
    severity,
    source: "act",
    actId: rule.actId,
    ruleType: rule.ruleType,
    wcagAll: rule.requirements
      .filter((r) => r.forConformance && /^\d/.test(r.sc))
      .map((r) => r.sc)
      .sort(cmpSc),
    applicability: clamp(rule.applicability),
    expectation: clamp(rule.expectation),
  };
}

// -----------------------------------------------------------------------------
// Load the vendored corpus into rows.
// -----------------------------------------------------------------------------

export function loadActRows(): AARuleRow[] {
  if (!existsSync(RULES_DIR)) {
    throw new Error(
      `ACT rules corpus not found at ${RULES_DIR}; run \`npm run ingest:act:download\`.`,
    );
  }
  const files = readdirSync(RULES_DIR)
    .filter((f) => f.endsWith(".md"))
    .sort();
  const rows: AARuleRow[] = [];
  for (const f of files) {
    const src = readFileSync(join(RULES_DIR, f), "utf8");
    const parsed = parseActRuleFile(f, src);
    rows.push(actRuleToRow(parsed));
  }
  return rows;
}

// -----------------------------------------------------------------------------
// `download` helper — refresh the vendored corpus from the pinned commit.
// Invoked via `npm run ingest:act:download`. The rule-file list is the set
// of proof rules ingested so far; extend it to grow the corpus.
// -----------------------------------------------------------------------------

const PROOF_RULE_FILES = [
  "button-non-empty-accessible-name-97a4e1.md",
  "html-page-lang-b5c3f8.md",
  "iframe-non-empty-accessible-name-cae760.md",
  "image-non-empty-accessible-name-23a2a8.md",
  "link-non-empty-accessible-name-c487ae.md",
];

async function download() {
  const { mkdirSync } = await import("node:fs");
  mkdirSync(RULES_DIR, { recursive: true });
  for (const f of PROOF_RULE_FILES) {
    const url = `https://raw.githubusercontent.com/${SOURCE_REPO}/${PINNED_COMMIT}/_rules/${f}`;
    const res = await fetch(url);
    if (!res.ok) throw new Error(`${f}: HTTP ${res.status}`);
    const text = await res.text();
    writeFileSync(join(RULES_DIR, f), text, "utf8");
    console.log(`downloaded ${f} (${text.length} bytes)`);
  }
}

// Run download only when invoked directly with `--download`.
if (process.argv[1]?.endsWith("act-rules.ts") && process.argv.includes("--download")) {
  download().catch((e) => {
    console.error(e);
    process.exit(2);
  });
}
