// Cribrum HTML model ingestion — emits src/Cribrum/Html/Model/Generated.idr
// from `ingest/content-model.ts` cross-validated against `@webref/elements`.
//
// `@webref/elements@2.6.0` ships only element *names* (and an `obsolete`
// flag); content-model shape, categories, and per-element attribute
// permissions live in WHATWG prose. So `ingest/content-model.ts` is the
// hand-curated structural data, and `@webref` is the upstream
// cross-validation source: every row in `content-model.ts` must
// correspond to a `@webref` element (invariant I8). When `@webref`
// matures its content-model coverage, rows shrink to override-only;
// shape of this script is unchanged.
//
// Run:
//   cd ingest && npm install && npm run ingest        # regenerate
//   cd ingest && npm install && npm run ingest:check  # gate (no write)

import { listAll } from "@webref/elements";
import { writeFileSync, readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { CONTENT_MODEL, GLOBAL_ATTRS, Category, ChildPolicy, ContentModelRow }
  from "./content-model.js";

// -----------------------------------------------------------------------------
// Invariant checks (run pre-emit).
// -----------------------------------------------------------------------------

function checkInvariants(rows: ContentModelRow[]): void {
  // I1: name uniqueness within content-model.ts.
  const seen = new Set<string>();
  for (const r of rows) {
    if (seen.has(r.name)) {
      throw new Error(`I1 violated: duplicate name "${r.name}" in content-model.ts`);
    }
    seen.add(r.name);
  }

  // I2: tag closure — every OnlyTags reference must exist as a row.
  for (const r of rows) {
    if (r.childPolicy.kind === "OnlyTags") {
      for (const t of r.childPolicy.tags) {
        if (!seen.has(t)) {
          throw new Error(
            `I2 violated: <${r.name}>'s OnlyTags references unknown tag "${t}"`);
        }
      }
    }
  }

  // I4: isVoid ⇒ childPolicy = NoChildren.
  for (const r of rows) {
    if (r.isVoid && r.childPolicy.kind !== "NoChildren") {
      throw new Error(
        `I4 violated: <${r.name}> isVoid=true but childPolicy=${r.childPolicy.kind}`);
    }
  }

  // I5: isRawText ⇒ childPolicy = TextOnly.
  for (const r of rows) {
    if (r.isRawText && r.childPolicy.kind !== "TextOnly") {
      throw new Error(
        `I5 violated: <${r.name}> isRawText=true but childPolicy=${r.childPolicy.kind}`);
    }
  }

  // I6: localAttrs ∩ globalAttrs disjoint (warning, not error).
  // `<body>`'s on* set is an intentional spec-fidelity exception (HTML
  // specifies them as `Window` event-handler attributes on body, so
  // they're listed here even though `on*` is globally tolerated by the
  // `isOnEventAttr` prefix rule).
  const globalSet = new Set(GLOBAL_ATTRS);
  for (const r of rows) {
    if (r.name === "body") continue;
    for (const a of r.localAttrs) {
      if (globalSet.has(a)) {
        console.warn(`I6 warning: <${r.name}> localAttr "${a}" is also a global attribute`);
      }
    }
  }
}

// I8: every content-model row corresponds to a known `@webref` element.
// Returns the upstream version label and the union of element names
// across all `@webref` specs (HTML + SVG + MathML + …).
async function loadWebref(): Promise<{ version: string; allNames: Set<string>; obsolete: Set<string> }> {
  const all = await listAll();
  const allNames = new Set<string>();
  const obsolete = new Set<string>();
  for (const spec of Object.values(all)) {
    for (const e of (spec as any).elements ?? []) {
      allNames.add(e.name);
      if (e.obsolete) obsolete.add(e.name);
    }
  }
  // Read @webref/elements package version from node_modules.
  const pkgPath = resolve(import.meta.dirname, "node_modules/@webref/elements/package.json");
  const pkg = JSON.parse(readFileSync(pkgPath, "utf8"));
  return { version: `@webref/elements@${pkg.version}`, allNames, obsolete };
}

function checkCrossSource(
  rows: ContentModelRow[],
  webrefNames: Set<string>,
  obsolete: Set<string>,
): void {
  // I8a: every row exists in @webref.
  for (const r of rows) {
    if (!webrefNames.has(r.name)) {
      throw new Error(`I8 violated: <${r.name}> in content-model.ts has no @webref entry`);
    }
    if (obsolete.has(r.name)) {
      console.warn(`I8 warning: <${r.name}> is marked obsolete by @webref`);
    }
  }
  // I8b: unmatched @webref names → informational warning only (Cribrum
  // catalog is intentionally a subset).
  const rowNames = new Set(rows.map(r => r.name));
  let unmatched = 0;
  for (const n of webrefNames) {
    if (!rowNames.has(n)) unmatched++;
  }
  if (unmatched > 0) {
    console.log(`@webref: ${unmatched} element(s) not in Cribrum catalog (informational)`);
  }
}

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

function emitSpec(s: ContentModelRow): string {
  const cats  = `[${s.categories.map(emitCategory).join(", ")}]`;
  // Sort localAttrs deterministically (I7).
  const sortedAttrs = [...s.localAttrs].sort();
  const attrs = `[${sortedAttrs.map(a => JSON.stringify(a)).join(", ")}]`;
  return `MkElementSpec ${JSON.stringify(s.name)} ${s.isVoid ? "True" : "False"} ${s.isRawText ? "True" : "False"} ${cats} ${emitChildPolicy(s.childPolicy)} ${attrs}`;
}

function emitModule(
  catalog: ContentModelRow[],
  webrefVersion: string,
  contentModelSha: string,
  catalogSha: string,
): string {
  // Sort lexicographically by name (I7 determinism).
  const sortedCatalog = [...catalog].sort((a, b) => a.name.localeCompare(b.name));
  const elementsList  = sortedCatalog.map((s, i) =>
    (i === 0 ? "  [ " : "  , ") + emitSpec(s)).join("\n");
  const sortedGlobal  = [...GLOBAL_ATTRS].sort();
  const globalAttrs   = sortedGlobal.map((a, i) =>
    (i === 0 ? "  [ " : "  , ") + JSON.stringify(a)).join("\n");
  return `||| AUTO-GENERATED by ingest/html-model.ts — do not edit by hand.
||| Source: ${webrefVersion}
||| content-model.ts: sha256 ${contentModelSha}
||| catalog hash: sha256 ${catalogSha}
module Cribrum.Html.Model.Generated

import Cribrum.Html.Category
import Cribrum.Html.Model.Types

%default total

public export
generatedElements : List ElementSpec
generatedElements =
${elementsList}
  ]

public export
generatedGlobalAttrs : List String
generatedGlobalAttrs =
${globalAttrs}
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
  const outputPath  = resolve(import.meta.dirname, "../src/Cribrum/Html/Model/Generated.idr");
  const cmSourcePath = resolve(import.meta.dirname, "content-model.ts");

  checkInvariants(CONTENT_MODEL);

  let webrefVersion: string;
  try {
    const wr = await loadWebref();
    webrefVersion = wr.version;
    checkCrossSource(CONTENT_MODEL, wr.allNames, wr.obsolete);
    console.log(`${webrefVersion}: ${wr.allNames.size} element(s) available`);
  } catch (e: any) {
    if (e?.message?.startsWith?.("I8 violated")) throw e;
    console.error("error: failed to load @webref/elements:", e);
    process.exit(2);
  }

  // Compute SHAs *after* validation so a passing run pins the inputs.
  const cmSource     = readFileSync(cmSourcePath, "utf8");
  const contentModelSha = sha256(cmSource);
  // Catalog hash is over the canonical JSON serialisation of the
  // sorted catalog — independent of source-file formatting.
  const sortedCatalog = [...CONTENT_MODEL].sort((a, b) => a.name.localeCompare(b.name))
    .map(r => ({ ...r, localAttrs: [...r.localAttrs].sort() }));
  const catalogSha   = sha256(JSON.stringify(sortedCatalog));

  const generated = emitModule(CONTENT_MODEL, webrefVersion, contentModelSha, catalogSha);

  if (check) {
    if (!existsSync(outputPath)) {
      console.error(`error: ${outputPath} does not exist; run \`npm run ingest\` to create.`);
      process.exit(1);
    }
    const existing = readFileSync(outputPath, "utf8");
    if (existing !== generated) {
      // Pinpoint which input drifted.
      const reEmbeddedCmSha = /content-model\.ts: sha256 ([0-9a-f]+)/.exec(existing)?.[1];
      const reEmbeddedCatSha = /catalog hash: sha256 ([0-9a-f]+)/.exec(existing)?.[1];
      const reEmbeddedVer   = /Source: (\S+)/.exec(existing)?.[1];
      console.error("error: generated module differs from checked-in copy.");
      if (reEmbeddedVer && reEmbeddedVer !== webrefVersion) {
        console.error(`  @webref drift: ${reEmbeddedVer} vs ${webrefVersion}`);
      }
      if (reEmbeddedCmSha && reEmbeddedCmSha !== contentModelSha) {
        console.error(`  content-model.ts drift: ${reEmbeddedCmSha} vs ${contentModelSha}`);
      }
      if (reEmbeddedCatSha && reEmbeddedCatSha !== catalogSha) {
        console.error(`  catalog hash drift: ${reEmbeddedCatSha} vs ${catalogSha}`);
      }
      console.error("  Run `npm run ingest` to refresh.");
      process.exit(1);
    }
    console.log("ingest round-trip OK");
  } else {
    writeFileSync(outputPath, generated, "utf8");
    console.log(`wrote ${outputPath}`);
  }
}

main().catch(e => { console.error(e); process.exit(2); });
