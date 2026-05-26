// Cribrum djot reference-corpus ingestion — downloads the pinned
// `jgm/djot.lua` test fixtures and rewrites them into the flat
// `=== input === / === expected ===` shape the Cribrum djotref gate
// expects (one test per file, in `test/djot-ref/corpus/`).
//
// `jgm/djot.lua` is the reference implementation. Its tests are the
// authoritative behaviour spec for the Djot constructs Cribrum must
// match; ingesting them replaces the 20-test hand-curated seed with
// the full reference corpus.
//
// Source format (`jgm/djot.lua/test/<topic>.test`): a file is prose
// mixed with fenced-test blocks. A test is a fenced block delimited
// by 3+ backticks (matching count on close), with `.` on its own line
// separating input from expected:
//
//     ```
//     <djot input>
//     .
//     <expected html>
//     ```
//
// Open fences may carry an annotation suffix (e.g. ```` ```m ````,
// ```` ```ap ````, ```` ```f ````) that requests AST-printed /
// source-position / filter modes Cribrum does not model. Annotated
// tests are skipped during ingestion. Plain tests become corpus
// files; the annotated ones never enter the gate.
//
// Run:
//   cd ingest && npm install && npm run ingest:djotref

import { writeFileSync, readdirSync, unlinkSync, mkdirSync, existsSync } from "node:fs";
import { resolve, join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname  = dirname(__filename);

// Pinned commit — bump deliberately. Drift between this constant and
// the upstream test corpus is the only knob; rerunning ingestion with
// a new commit is a content change reviewed in PR.
const PINNED_COMMIT = "cbd34858ee7d5d076663d3e39362fb080b55c1df";
const SOURCE_REPO   = "jgm/djot.lua";

// Files that contain executable tests. `sourcepos.test` is intentionally
// excluded — every fixture in that file requests the `m` source-position
// mode Cribrum doesn't model. `filters.test` is excluded for the same
// reason (Lua-filter-only).
const TEST_FILES = [
  "attributes",
  "blockquote",
  "code_blocks",
  "definition_lists",
  "emphasis",
  "escapes",
  "fenced_divs",
  "footnotes",
  "headings",
  "insert_delete_mark",
  "links_and_images",
  "lists",
  "math",
  "para",
  "raw",
  "regression",
  "smart",
  "spans",
  "super_subscript",
  "symbol",
  "tables",
  "task_lists",
  "thematic_breaks",
  "verbatim",
];

interface ExtractedTest {
  annotation: string;   // text on the opening fence after the backticks
  input:      string;
  expected:   string;
}

function extractTests(src: string): ExtractedTest[] {
  const lines = src.split("\n");
  const tests: ExtractedTest[] = [];
  let i = 0;
  while (i < lines.length) {
    const line  = lines[i];
    const open  = line.match(/^(`{3,})(.*)$/);
    if (!open) { i++; continue; }
    const fence      = open[1];
    const annotation = open[2].trim();
    i++;
    const inputLines    : string[] = [];
    const expectedLines : string[] = [];
    let sawDot     = false;
    let sawClose   = false;
    while (i < lines.length) {
      const inner = lines[i];
      if (inner === fence) { sawClose = true; i++; break; }
      if (!sawDot && inner === ".") { sawDot = true; i++; continue; }
      (sawDot ? expectedLines : inputLines).push(inner);
      i++;
    }
    if (!sawClose) continue;   // unterminated fence — bail on this block
    if (!sawDot)   continue;   // plain code block (no `.` separator), not a test
    tests.push({
      annotation,
      input:    inputLines.join("\n") + (inputLines.length ? "\n" : ""),
      expected: expectedLines.join("\n") + (expectedLines.length ? "\n" : ""),
    });
  }
  return tests;
}

async function fetchTestFile(topic: string): Promise<string> {
  const url = `https://raw.githubusercontent.com/${SOURCE_REPO}/${PINNED_COMMIT}/test/${topic}.test`;
  const r = await fetch(url);
  if (!r.ok) throw new Error(`HTTP ${r.status} for ${url}`);
  return await r.text();
}

function emit(corpus: string, topic: string, idx: number, t: ExtractedTest): void {
  const padded = idx.toString().padStart(3, "0");
  const slug   = topic.replace(/_/g, "-");
  const path   = join(corpus, `${slug}-${padded}.test`);
  const body   = `=== input ===\n${t.input}=== expected ===\n${t.expected}`;
  writeFileSync(path, body);
}

async function main(): Promise<void> {
  const repoRoot = resolve(__dirname, "..");
  const corpus   = join(repoRoot, "test", "djot-ref", "corpus");
  if (!existsSync(corpus)) mkdirSync(corpus, { recursive: true });

  // Full replacement — the hand-curated seed is superseded by the
  // reference corpus. The djotref baseline (`test/djot-ref/baseline.txt`)
  // is regenerated post-ingest via `make djotref-update`.
  for (const f of readdirSync(corpus)) {
    if (f.endsWith(".test")) unlinkSync(join(corpus, f));
  }

  let wrote = 0;
  let skippedAnnotated = 0;
  for (const topic of TEST_FILES) {
    let src: string;
    try {
      src = await fetchTestFile(topic);
    } catch (e) {
      console.error(`  ! ${topic}: ${(e as Error).message}`);
      continue;
    }
    const tests = extractTests(src);
    let idx = 0;
    for (const t of tests) {
      idx++;
      if (t.annotation !== "") { skippedAnnotated++; continue; }
      emit(corpus, topic, idx, t);
      wrote++;
    }
    console.log(`  ${topic}: +${tests.filter(t => t.annotation === "").length} (-${tests.filter(t => t.annotation !== "").length} annotated)`);
  }

  console.log(`\ndjot-ref ingest: wrote ${wrote} tests (skipped ${skippedAnnotated} annotated) from ${SOURCE_REPO}@${PINNED_COMMIT.slice(0, 12)}`);
}

main().catch(e => { console.error(e); process.exit(1); });
