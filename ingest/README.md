# Cribrum spec ingestion

Per `plan.dj` §P2.1 / §P3.1: the HTML element model and the WCAG AA
rule catalog are large external specifications. Cribrum ingests them
from machine-readable upstreams rather than hand-transcribing them.

## html-model.ts

Pulls element data from [`@webref/elements`][webref] (the WHATWG /
W3C machine-readable element/attribute schema) and emits
`src/Cribrum/Html/Model/Generated.idr`.

```
cd ingest
npm install
npm run ingest         # regenerate Generated.idr
npm run ingest:check   # gate: error if Generated.idr is stale
```

### Round-trip discipline

The committed `src/Cribrum/Html/Model.idr` is the **authoritative**
catalog interpreted by the Phase-2 validator. The script's
`Generated.idr` output is the **regeneration target**: re-running the
script must produce a file equal to a previous run's output. CI is
expected to run `npm run ingest:check`; drift fails the gate.

The hand-curated `Cribrum.Html.Model` and the script's seed catalog
are currently synchronised manually. As `@webref/elements` matures its
content-model coverage, the script will compile the catalog directly
from upstream data rather than carrying the seed.

[webref]: https://github.com/w3c/webref

## aa-catalog.ts + act-rules.ts

Compiles the merged AA catalog into
`src/Cribrum/AA/Catalog/Generated.idr`. Two sources feed it:

- **`ingest/aa.ts`** — hand-curated Cribrum rules (`source: "cribrum"`):
  id, WCAG SC, level, title, confidence, severity.
- **`ingest/act-rules.ts`** — the W3C-CG **ACT-rules** upstream pull
  (`source: "act"`, plan §P3.1). It parses the YAML front-matter +
  `## Applicability` / `## Expectation` prose of the rule `.md` files
  vendored under `ingest/act-rules/_rules/` and maps each rule into the
  same `AARuleRow` shape (primary WCAG SC + level, ACT id, rule_type,
  every forConformance SC, and the applicability/expectation prose as
  data fields).

The auto-generated module exposes one `Rule` constant per row plus a
`generatedRules : List Rule` that `Cribrum.AA.Catalog` re-exports as
`allRules`.

```
cd ingest
npm install
npm run ingest:act:download   # refresh vendored ACT corpus (network)
npm run ingest:aa             # regenerate Generated.idr (aa.ts + ACT)
npm run ingest:aa:check       # gate: error if Generated.idr is stale
```

The combined `npm run ingest` / `npm run ingest:check` run both the
HTML-model and AA-catalog gates back-to-back, and `make ingest-check`
is the project-level entry point.

### ACT-rules pipeline (plan §P3.1)

Source: `github.com/act-rules/act-rules.github.io`, `_rules/*.md`. The
rule files are **vendored** at a pinned commit (`PINNED_COMMIT` in
`act-rules.ts`) so `make ingest` runs offline — mirroring `djot-ref.ts`.
Refreshing the corpus is a deliberate content change: bump the commit,
`npm run ingest:act:download`, rerun ingest, review the catalog diff.

**Growing the catalog is now data-entry, not Idris:** drop a rule's
`.md` under `ingest/act-rules/_rules/` (or add it to `PROOF_RULE_FILES`
and re-download), rerun `npm run ingest`. The new row flows into
`Generated.idr` and the `Cribrum.AA.Partition` audit pins the rule-id
set so every addition lands explicitly.

Classification today: ACT rows land **Heuristic** (pass-only, never
claims conformance) because their expectations are accessible-name /
accessibility-tree predicates not yet statically decidable on Cribrum's
HTML tree alone. The `applicability` / `expectation` fields are carried
as text so a follow-up can promote the tree-decidable ones to
`Structural` and graduate them to typed propositions in
`Cribrum.AA.Pass` / `Cribrum.AA.Typed`.

## djot-ref.ts

Downloads the pinned `jgm/djot.lua` test corpus and rewrites each
fenced test into the flat `=== input === / === expected ===` shape
the djotref gate consumes. Replaces the seed corpus in
`test/djot-ref/corpus/` wholesale.

```
cd ingest
npm install
npm run ingest:djotref     # re-download + regenerate corpus
make djotref-update        # refresh baseline.txt with the new pass set
```

Bumping the upstream commit is a one-line edit to `PINNED_COMMIT` in
`djot-ref.ts`. Annotated tests (`m`, `ap`, `f`, ...) request optional
features Cribrum doesn't model (source positions, AST printing, Lua
filters) and are skipped during ingestion; counts for both kept and
skipped tests are printed.

## oracle.ts

Cribrum's side of the plan §P2.4 oracle. Reads the JSONL corpus
emitted by `tools/oracle-emit/` (one row per `(name, html, decided,
expected)` curated case) and validates each `html` against the W3C
Nu Html Checker (`vnu.jar`, shipped via the `vnu-jar` npm package).
Reports divergence across three sources: curator's expected verdict,
Cribrum's `decideHtml`, and vnu's verdict — all three must agree.

```
make oracle
# or, equivalently:
pack -q run tools/oracle-emit/oracle-emit.ipkg | npx tsx oracle.ts
```

vnu needs Java on `$PATH`. If either Java or the vnu shim is missing
the script prints an actionable skip message and exits 0. Set
`CRIBRUM_ORACLE_REQUIRE_VNU=1` to turn a missing dependency into a
hard failure (CI uses this).
