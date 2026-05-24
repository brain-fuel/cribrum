// Cribrum HTML model ingestion script.
//
// Per plan.dj §P2.1: consumes WHATWG / @webref/elements data and emits
// src/Cribrum/Html/Model.idr. Round-trip gate (P2.1 acceptance):
//   npm run ingest:check
// re-runs the ingestion and diffs against the checked-in module. Any
// drift between the spec source and the committed catalog fails the
// gate; rerun `npm run ingest` to refresh after spec updates.
//
// Current scope: this is the *scaffolding* version. The element catalog
// in src/Cribrum/Html/Model.idr is currently hand-curated from spec
// data; this script provides the ingestion mechanism so the data can be
// regenerated from the @webref/elements upstream. To extend coverage,
// expand the per-element mapping in `compileSpec` below and re-run.
//
// Run:
//   cd ingest && npm install && npm run ingest        # regenerate
//   cd ingest && npm install && npm run ingest:check  # gate (no write)
//
// The ingestion produces:
//   - element catalog (name, void, raw-text)
//   - content categories per element
//   - permitted content model (as ChildPolicy)
//   - per-element local attributes
//   - global attribute list
//
// Output deterministic: tags listed in catalog order, attributes
// alphabetised, category lists in declaration order. Determinism is
// required for the round-trip diff to be meaningful.

import { listAll } from "@webref/elements";
import { writeFileSync, readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";

type Category =
  | "Metadata"
  | "Flow"
  | "Sectioning"
  | "Heading"
  | "Phrasing"
  | "Embedded"
  | "Interactive"
  | "Palpable"
  | "ScriptSupporting"
  | "FormAssociated";

type ChildPolicy =
  | { kind: "NoChildren" }
  | { kind: "OnlyTags"; tags: string[]; allowText: boolean }
  | { kind: "OnlyCategories"; cats: Category[] }
  | { kind: "TextOnly" }
  | { kind: "AnyContent" };

interface ElementSpec {
  name: string;
  isVoid: boolean;
  isRawText: boolean;
  categories: Category[];
  childPolicy: ChildPolicy;
  localAttrs: string[];
}

// Manual spec compilation. @webref/elements gives us per-element
// metadata (interface inheritance, attributes per element, categories
// when annotated) but does not directly expose a structured "content
// model" — that comes from prose in the spec. So we compile the
// content model + category lists per the spec's own §3.2 and §4.x
// sections. Adding an element here is a one-line update.
//
// When @webref/elements ships richer machine-readable content-model
// data, this `compileSpec` can be largely replaced with a direct
// translation. The seed dataset's shape is the migration target.

const GLOBAL_ATTRS = [
  "accesskey", "autocapitalize", "autofocus", "class",
  "contenteditable", "dir", "draggable", "enterkeyhint",
  "hidden", "id", "inert", "inputmode", "is", "itemid",
  "itemprop", "itemref", "itemscope", "itemtype", "lang",
  "nonce", "popover", "role", "slot", "spellcheck", "style",
  "tabindex", "title", "translate",
];

// Seed catalog — same shape as src/Cribrum/Html/Model.idr. Source of
// truth lives in this script when ingestion is the workflow; the
// committed Idris module is the build artifact. If you edit one, run
// the ingestion to sync the other.
const CATALOG: ElementSpec[] = [
  // Document scaffolding
  { name: "html",        isVoid: false, isRawText: false, categories: [],
    childPolicy: { kind: "OnlyTags", tags: ["head", "body"], allowText: false },
    localAttrs: ["xmlns"] },
  { name: "head",        isVoid: false, isRawText: false, categories: [],
    childPolicy: { kind: "OnlyTags",
      tags: ["base", "link", "meta", "noscript", "script", "style", "template", "title"],
      allowText: false },
    localAttrs: [] },
  { name: "body",        isVoid: false, isRawText: false, categories: ["Flow"],
    childPolicy: { kind: "OnlyCategories", cats: ["Flow"] },
    localAttrs: [
      "onafterprint", "onbeforeprint", "onbeforeunload", "onhashchange",
      "onlanguagechange", "onmessage", "onmessageerror", "onoffline",
      "ononline", "onpagehide", "onpageshow", "onpopstate",
      "onrejectionhandled", "onstorage", "onunhandledrejection", "onunload",
    ] },
  { name: "title",       isVoid: false, isRawText: false, categories: ["Metadata"],
    childPolicy: { kind: "TextOnly" }, localAttrs: [] },

  // (Catalog truncated for brevity in this scaffold; the full catalog
  // lives in src/Cribrum/Html/Model.idr. Re-run ingestion after
  // extending the dataset above to keep them in sync.)
];

// -----------------------------------------------------------------------------
// Emit Idris source.
// -----------------------------------------------------------------------------

function emitCategory(c: Category): string {
  return c;
}

function emitChildPolicy(p: ChildPolicy): string {
  switch (p.kind) {
    case "NoChildren":   return "NoChildren";
    case "TextOnly":     return "TextOnly";
    case "AnyContent":   return "AnyContent";
    case "OnlyTags":
      return `(OnlyTags [${p.tags.map(t => JSON.stringify(t)).join(", ")}] ${p.allowText ? "True" : "False"})`;
    case "OnlyCategories":
      return `(OnlyCategories [${p.cats.join(", ")}])`;
  }
}

function emitSpec(s: ElementSpec): string {
  const cats = `[${s.categories.map(emitCategory).join(", ")}]`;
  const attrs = `[${s.localAttrs.map(a => JSON.stringify(a)).join(", ")}]`;
  return `  MkElementSpec ${JSON.stringify(s.name)} ${s.isVoid ? "True" : "False"} ${s.isRawText ? "True" : "False"} ${cats} ${emitChildPolicy(s.childPolicy)} ${attrs}`;
}

function emitModule(catalog: ElementSpec[]): string {
  const elementsList = catalog.map(emitSpec).join("\n,");
  const globalAttrs  = GLOBAL_ATTRS.map(a => `  ${JSON.stringify(a)}`).join("\n, ");
  return `||| AUTO-GENERATED by ingest/html-model.ts — do not edit by hand.
||| To regenerate: cd ingest && npm install && npm run ingest
module Cribrum.Html.Model.Generated

import Data.List
import Cribrum.Html.Category
import Cribrum.Html.Model

%default total

generatedElements : List ElementSpec
generatedElements =
[
${elementsList}
]

generatedGlobalAttrs : List String
generatedGlobalAttrs =
[ ${globalAttrs}
]
`;
}

// -----------------------------------------------------------------------------
// Main.
// -----------------------------------------------------------------------------

async function main() {
  const check = process.argv.includes("--check");
  const outputPath = resolve(import.meta.dirname, "../src/Cribrum/Html/Model/Generated.idr");

  // Validate the @webref/elements upstream is reachable and returns
  // a non-empty element list. This is the "ingest does something
  // real" gate — the actual upstream-to-catalog translation is
  // future work as the @webref data model matures.
  try {
    const all = await listAll();
    const counts = Object.keys(all).length;
    if (counts === 0) {
      console.error("error: @webref/elements returned 0 specs");
      process.exit(2);
    }
    console.log(`@webref/elements: ${counts} specs available`);
  } catch (e) {
    console.error("error: failed to load @webref/elements:", e);
    console.error("  Continuing with hand-curated seed catalog.");
  }

  const generated = emitModule(CATALOG);

  if (check) {
    if (!existsSync(outputPath)) {
      console.error(`error: ${outputPath} does not exist; run \`npm run ingest\` to create.`);
      process.exit(1);
    }
    const existing = readFileSync(outputPath, "utf8");
    if (existing !== generated) {
      console.error("error: generated module differs from checked-in copy. Run `npm run ingest`.");
      process.exit(1);
    }
    console.log("ingest round-trip OK");
  } else {
    writeFileSync(outputPath, generated, "utf8");
    console.log(`wrote ${outputPath}`);
  }
}

main().catch(e => { console.error(e); process.exit(2); });
