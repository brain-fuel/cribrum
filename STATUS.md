# Cribrum — Current Status

> Snapshot of what's built. Authoritative narrative is `plan.dj`; the
> convention catalog is `docs/conventions.md`.

## Modules

| Module                    | Phase | Status |
|---------------------------|-------|--------|
| `Cribrum.Node`            | core  | HExpr IR (Element/Text/Comment, HAttr w/ handler-capable AttrValue, traversal). |
| `Cribrum.Djot.Surface`    | 1a    | Faithful Djot AST — full construct inventory representable. |
| `Cribrum.Djot.Parser`     | 1a    | **Slice**: paragraph, ATX heading (1-6), thematic break, block quote (recursive), fenced code block. Lists, inline emphasis parsing, tables → deferred. |
| `Cribrum.Html.Category`   | 2     | Content-category enum (Metadata/Flow/Sectioning/Heading/Phrasing/Embedded/Interactive/Palpable/ScriptSupporting/FormAssociated). |
| `Cribrum.Html.Model`      | 2     | Public surface (types + lookups + attribute-name permission). Element catalog (114 elements) lives in `Cribrum.Html.Model.Generated`, ingested from `ingest/content-model.ts` by `ingest/html-model.ts` with `@webref/elements@2.6.0` cross-validation (invariant I8). `Cribrum.Html.Model.Types` carries `ElementSpec` / `ChildPolicy` (extracted to break the Model ↔ Generated cycle). `Cribrum.Html.Model.Invariants` lifts the TS-side tag-closure check (I2) to a decidable Idris proposition (`AllChildTagsExist` + `decAllChildTagsExist`); the test suite asserts the real catalog satisfies it. Drift gate `make ingest-check` fails on `@webref` bump, `content-model.ts` edit, or hand-edit to `Generated.idr` and pinpoints which input drifted. |
| `Cribrum.Html.Valid`      | 2     | `IsValidHtml = IsKnownTag × All AttrAllowedIn × All ChildAllowedIn × All IsValidHtml`. Total `decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`. Located rejection (`decideHtmlLocated`) returns path-into-tree + `RejectionClass` (UnknownTag / DisallowedAttr / IllegalChild / BlockInPhrasing / MalformedTable / TextNotAllowedIn / CommentNotAllowedIn). |
| `Cribrum.Elaborate`       | 1b    | Strict elaboration `Doc -> Either ElabError (h ** (IsValidHtml h, StructuralAA h))`. Phase-2 sharpened: failure path uses `LocatedHtmlError` carrying a `LocatedReject`. **Phase-4 sharpened**: `StructuralAA h` is now the actual conjunct of all 10 Phase-4 propositions (img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend, button-name, link-name, document-lang, heading-no-skip, duplicate-id) — `decStructuralAA` short-circuits to `StructuralAaFailure ruleId path` on the first failing predicate. AA failure-path **located**: per-node rules carry `Just path`; root-only `document-lang` carries `Just []`; whole-tree rules (heading-no-skip, duplicate-id) carry `Nothing`. |
| `Cribrum.Render.Html`     | 5     | Total `HExpr -> String`. HTML 5 void-element handling, escaping, handler attrs render as `data-on-<event>`. |
| `Cribrum.Render.Dom`      | 5     | **Spike**: tiny FFI surface (createElement/createTextNode/createComment/setAttribute/removeAttribute/addEventListener/appendChild/replaceChild/getElementById/clearChildren) + `currentEventValue` (input-value extraction) + `captureFocus`/`restoreFocus` (focus + selection-range preservation across reconcile). `renderDom : HExpr -> IO DomNode`, `reconcile` (Day-1 blow-and-rebuild, skip on unchanged tree, focus bracketed), `mountInto`. JS-backend only at runtime; chez type-checks via multi-spec %foreign with a scheme: fallback. Handler attrs dispatch via `window.__cribrumDispatch`; the dispatcher pre-extracts `event.target.value` into `window.__cribrumValue` for `onInput`/`onChange`. |
| `Cribrum.AA.Catalog`      | 3+4   | Shared rule catalog (single source of truth across pass + future types). Types (`Rule`/`Confidence`/`Severity`) in `Cribrum.AA.Catalog.Types`; rule data in `Cribrum.AA.Catalog.Generated`, ingested from `ingest/aa.ts` by `ingest/aa-catalog.ts` (plan §P3.1 scaffold) with `make ingest-check` drift gate. 11 rules: img-alt, anchor-href, alt-meaningful, heading-no-skip, document-lang, iframe-title, label-for-control, fieldset-legend, link-name, button-name, duplicate-id. |
| `Cribrum.AA.Pass`         | 3     | Per-rule traversal: 4 spike rules + 7 added (document-lang root check, iframe-title, label-for-control with implicit-control support, fieldset-legend, link-name with `aria-label`/`title`/text fallbacks, button-name, duplicate-id whole-tree). Total. Confidence-partitioned (Structural / Heuristic). Data-interpreter refactor still ahead — current style is per-rule hand-written walkers. |
| `Cribrum.AA.Typed`        | 4     | **All 10 Structural rules** from the catalog promoted to type-level propositions via `So`: img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend, button-name, link-name (per-node `All` over `walkNodes`); document-lang (root-only); heading-no-skip, duplicate-id (whole-tree bool + `So`). Decision via `decSo` (and `All` for per-node rules). Each per-node rule's typed wrapper is a 5-line alias through `Cribrum.AA.Promote`. |
| `Cribrum.AA.Promote`      | 4     | Per plan §P4.2 the bool-predicate + `So` + `All` over `walkNodes` pattern is factored into a single generic interface: `NodeOk pred`, `AllNodesOk pred`, `decAllNodesOk pred`, `allNodesOk pred`. Per-node rules in `Cribrum.AA.Typed` consume it; adding a new per-node rule is now ~5 lines. |
| `TEAWeb.Html`             | T1    | **Spike**: view-builder smart constructors for 34 elements; `Attr msg` data type (`Plain` / `On`); `View msg` record = (HExpr, HandlerTable with `Event -> IO msg` closures); leaf nodes (`text_`, `comment_`); void elements (`br_`, `hr_`); plain attribute helpers (`class_`, `id_`, `href_`, ...). `viewSafe` routes through Phase-2's `decideHtmlLocated`, returning `Either ViewError ((h ** IsValidHtml h), HandlerTable)` with `LocatedReject` on rejection — content-model + attribute-permission misuse is now caught dynamically with path-into-tree diagnostics. Statically-typed-by-construction constructors (e.g. `ul_ : List (h ** IsLiChild h) -> View msg`) are the planned post-Phase-2 follow-on; the dynamic gate covers the contract until that lands. `eventTargetValue` re-exports the value-extraction primitive from `Cribrum.Render.Dom`. |
| `TEAWeb.Event`            | T2    | **Spike**: `onClick`/`onSubmit`/`onFocus`/`onBlur`/`onDoubleClick`/`onMouseEnter`/`onMouseLeave` (msg-form, IO-wrapped via `pure`); `onInput`/`onChange` (String-callback form; the closure runs `eventTargetValue` to read the pre-extracted `event.target.value`). Callback ids app-supplied for MVP; deterministic `hash(path, event)` when keyed diff lands. |
| `TEAWeb.Program`          | T3    | **Spike**: `Program model msg` record (init/update/view/subscriptions). |
| `TEAWeb.Cmd`              | T4    | **Spike**: `None`/`Batch`/`Focus`/`Blur`. `flatten : Cmd msg -> List (Cmd msg)`. Http/Random/After deferred to TEAWeb T6 demo's needs. |
| `TEAWeb.Sub`              | T4    | **Spike**: `None`/`Batch` only. Leaf variants (`OnKeyDown`, `OnAnimationFrame`, `Every`, `OnResize`, `Port`) deferred to T6 (Counter+Focus doesn't need them). |
| `TEAWeb.Runtime`          | T3+T4 | **Spike**: `mount` + tail-recursive interpreter loop in Idris; `installDispatch` installs single global `window.__cribrumDispatch`; `runCmd` interprets Focus/Blur via FFI; reconcile after each update keeps state ref + handler table in lockstep. JS-backend execution only. |
| `TEAWeb.Ports`            | T5    | Not started. Typed JSON FFI boundary for app-specific JS. |

## Tests (326 total, all green)

```
$ make test-fast        # cribrum + teaweb suites
$ make test             # adds ingest drift gate + mutation gate
```

Counts per group: 18 Node + 16 Surface + 57 Parser + 20 Model + 39 Valid
+ 17 Elaborate + 17 Render.Html + 1 Render.Dom + 34 AA.Pass + 68 AA.Typed
+ 4 Integration (README.dj + plan.dj) = 291 Cribrum. 10 TEAWeb.Html
+ 10 TEAWeb.Event + 6 TEAWeb.Cmd + 9 TEAWeb.Program = 35 TEAWeb.

Each module has:
- **EXTs** (example tests) — canonical cases for each behaviour.
- **PDDTs** (parameterised data-driven tests) — tables sweeping the input
  space.
- **PBTs** (property-based tests via hedgehog) — invariants over generated
  inputs.

## Mutation gate (124 mutants, 0 surviving)

```
$ test/mutation/run.sh                # changed-file scope (default)
$ MUTATION_BASE=ALL test/mutation/run.sh   # full sweep
```

Data-driven mutation suite in `test/mutation/mutants.tsv`. Each row is
`FILE\tFIND\tREPLACE\tDESCRIPTION`. The runner:

1. Wipes the cribrum install cache + local build dirs (pack's
   content-hash install would otherwise serve stale `.ttc` for the
   same source content from a previous iteration).
2. Mutates one source file (literal substitution).
3. Reinstalls cribrum, runs the test suite.
4. A passing test run = mutant **survived** (bad).
5. Restores source, repeats.

Approved-survivor mechanism: `APPROVED="3 7" test/mutation/run.sh` excludes
indices 3 and 7 from the failure count (reserved for semantically
equivalent mutants — none used yet).

Gate: zero non-approved survivors.

## Pipeline

```
.dj source
  └── parseDoc      (Cribrum.Djot.Parser)      -> Doc
       └── elaborate (Cribrum.Elaborate)        -> (h ** IsValidHtml h × StructuralAA h)
            ├── renderHtml (Cribrum.Render.Html) -> String
            └── checkAA    (Cribrum.AA.Pass)     -> AAReport
```

The integration tests in `Test.Cribrum.Integration` run `README.dj` *and*
`plan.dj` through the whole pipeline and assert:
- Every heading + thematic break + `<main>` landmark survives.
- No bare top-level `<div>` (the "no div/span soup" commitment).
- The dependent pair carries a real `IsValidHtml h` witness.

`plan.dj` is the project's authoritative architectural narrative, dogfooded
through Cribrum's own pipeline (matching `README.dj`'s role).

## What's NOT shipped yet

- **Phase 1a parser**: lists, tables, inline emphasis/links/images/footnotes
  parsing, generic attribute block parsing, reference link definitions, raw
  blocks, smart-punctuation tokenisation. (Block quote, fenced code block,
  paragraph, heading, thematic break already shipped.)
- **Phase 2 ingestion + statically-typed view constructors**: the
  catalog + content-model + attribute-permission validator are in
  place (`Cribrum.Html.Model` + `Cribrum.Html.Valid`), with located
  rejection. **`@webref/elements`-driven ingestion shipped**: the
  catalog data lives in `ingest/content-model.ts`, compiled by
  `ingest/html-model.ts` to `Cribrum.Html.Model.Generated`;
  `@webref/elements@2.6.0` (pinned, lockfile committed) provides
  name-closure cross-validation (invariant I8) and obsoleted-element
  flagging. Pre-emit invariants I1/I2/I4/I5/I6/I8 enforced in TS;
  I7 (deterministic byte-identical emission) covered by `make
  ingest-check` (header carries `@webref` version +
  `content-model.ts` sha + catalog hash, so drift messages name which
  input changed). Tag-closure (I2) additionally lifted to a typed
  proposition (`Cribrum.Html.Model.Invariants.decAllChildTagsExist`)
  with module-load test on the real catalog. Statically-typed TEAWeb
  smart constructors (`ul_ : List (h ** IsLiChild h) -> View msg`)
  remain deferred — dynamic `viewSafe` covers the contract today.
- **Phase 3 catalog**: 11 of ~50 WCAG AA success criteria currently
  modelled (img-alt, anchor-href, alt-meaningful, heading-no-skip,
  document-lang, iframe-title, label-for-control, fieldset-legend,
  link-name, button-name, duplicate-id). Plan calls for ACT-rule
  ingestion to drive the full catalog; `ingest/aa.ts` is the
  scaffold target. Pass implementation is still per-rule hand-
  written; data-interpreter refactor (plan §P3.2) tracked separately.
- **Phase 4**: all 10 Structural AA rules in the catalog are now promoted
  to types (img-alt, anchor-href, iframe-title, label-for-control,
  fieldset-legend, button-name, link-name, document-lang, heading-no-
  skip, duplicate-id) AND the `Cribrum.AA.Promote` factor (plan §P4.2)
  is in place, compressing each per-node rule's typed wrapper to ~5
  lines. **Plan §P4.3 (elaboration parity) landed**: `Cribrum.Elaborate`'s
  `StructuralAA` codomain is now the real conjunct of all 10 propositions,
  decided by `decStructuralAA`; strict `elaborate` fails hard with
  `StructuralAaFailure ruleId path` on the first failing predicate. AA
  failures are **located**: per-node rules carry `Just path` (via
  `Promote.pathOfFirstFailing`), root-only `document-lang` carries
  `Just []`, and whole-tree rules (heading-no-skip, duplicate-id) carry
  `Nothing`. The PBT generator `Test.Cribrum.Elaborate.genSimpleBlocks`
  normalises heading-level sequences so generated docs always satisfy
  heading-no-skip.
- **Phase 5 DOM render execution**: FFI surface + `renderDom` +
  `reconcile` chez-type-checked AND JS-bundled via the
  `examples/teaweb/counter` MVP demo. Browser execution validated
  manually by loading `examples/teaweb/counter/index.html` (no
  automated browser test in CI yet). `removeEventListener` deferred
  to keyed-children diff.
- **Convention layer**: §2 of `docs/conventions.md` is mostly deferred —
  elaboration currently only wraps blocks in `<main>`; `:::nav` etc. don't
  promote yet.
- **Ingest pipeline**: Both gates shipped — HTML content model (114
  elements, `Cribrum.Html.Model.Generated` from `ingest/content-model.ts`
  + `@webref/elements@2.6.0` cross-validation) AND the AA catalog
  scaffold (11 rules, `Cribrum.AA.Catalog.Generated` from `ingest/aa.ts`
  via `ingest/aa-catalog.ts`). `make ingest-check` covers both. ACT-rule
  upstream pull + applicability/expectation data are still ahead — the
  scaffold matches the existing `Rule` shape so the upstream pull lands
  as content on `aa.ts` rather than a new pipeline. Catalog growth
  (~50 SCs / ~35 ACT rules) + `Cribrum.AA.Pass` data-interpreter refactor
  (plan §P3.2) remain.
- **Phase T (TEAWeb) full inventory**: MVP slice ships (Html, Event,
  Cmd None/Batch/Focus/Blur, Sub None/Batch, Program, Runtime, plus
  examples/teaweb/counter demo). Still missing: Sub leaf variants
  (OnKeyDown/Every/OnAnimationFrame/Port), Cmd Http/Random/After,
  Ports module, post-Phase-2 typed view-builders, post-Phase-4
  StructuralAA-in-view-codomain, keyed-children reconcile, T6 docs
  nav demo.
