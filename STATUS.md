# Cribrum — Current Status

> 512 tests across 19 groups, all green. Phase 4 typed-by-construction
> view constructors landed (`TEAWeb.Html.Typed` — Step 4). AA catalog
> grew from 12 → 19 rules (Step 5 partial): area-alt, link-empty-href,
> meta-no-refresh, summary-not-empty, track-kind (all Structural, all
> promoted to types); aria-label-redundant + positive-tabindex
> (Heuristic). **Pass.idr data-interpreter refactor landed (plan §P3.2)**:
> `Cribrum.AA.Pass.nodeRuleImpls` is a `List (Rule, NodeRuleCheck)`
> registry the per-node traversal folds over (16 rows); `treeRuleImpls`
> mirrors the same shape for the three whole-tree rules. Adding a per-
> node rule = one row pairing the catalog `Rule` with its impl.
> **T6 docs-nav demo now drives off the actual pipeline (plan §T6
> acceptance met)**: new `Cribrum.Pipeline.Anchor` (slugify + per-tree
> heading-id rewriter + harvester) plus `tools/render-docsnav/` emit
> both `examples/teaweb/docsnav/index.html` (the prose body, doctype-
> wrapped) and `examples/teaweb/docsnav/src/Generated.idr` (the TOC
> records the island consumes). Anchors are slugified once and shared
> between `<h2>`/`<h3>` ids and TOC `href`s, so in-page navigation works
> without manual sync; the previously hand-coded `tocItems` table in
> `Main.idr` is gone. `make docsnav` regenerates both artifacts and the
> JS bundle. **README.dj → HTML** via `make readme` runs the same
> `parseDoc -> elaborate -> renderHtml` pipeline end-to-end and writes
> `README.html` (proof-carrying tree). T6 docs nav island shipped (Step 7) —
> `examples/teaweb/docsnav/`. `TEAWeb.Event` gained `onKeyDown` /
> `onKeyUp` + supporting FFI in `Cribrum.Render.Dom.currentEventKey`.
> **Step 8 parser slice** (P5.4 partial): inline emphasis (`_em_`),
> strong (`*strong*`), verbatim (`` `code` ``), inline links
> (`[text](url)`); unordered lists (`-`/`*`/`+` markers) and ordered
> lists (`<n>.` markers). Elaborator maps each to its semantic HTML
> tag (`em`/`strong`/`code`/`a[href]`/`ul`/`ol`). **Step 9 parser
> slice**: inline images (`![alt](src)` — elaborator emits void
> `<img alt src>` with alt = plain-text flattening of parsed alt
> children, satisfying the structural `img-alt` rule by construction);
> autolinks (`<url>` / `<email>` — guarded by a non-empty body
> heuristic requiring `:` or `@` and no whitespace; falls back to
> literal text otherwise so prose like `<foo>` stays prose); hard
> breaks (trailing `\` on a non-last paragraph line strips the `\`
> and emits `InlHardBreak` instead of `InlSoftBreak`; trailing `\`
> at end-of-paragraph stays literal). **Integration suite now reads
> the real `README.dj` + `plan.dj` files** at test time (in
> `test/src/Main.idr`) rather than embedded slices — the previous
> stale-fixture hazard is gone, and the integration suite gains a
> determinism check on the full pipeline. **Step 10 parser slice**:
> smart punctuation (`--`/`---`/`...` and orientation-aware curly
> quotes), pipe tables with optional alignment row + header detection
> (elaborator emits a real `<table>`/`<thead>`/`<tbody>` tree with
> `style="text-align:..."` per-cell when alignments are declared),
> reference links + definitions (`[text][ref]`, `[text][]`, and
> `[ref]: url "title"` definitions; a post-parse resolver rewrites
> `LinkReference` to `LinkInline` when the label is defined and
> leaves unresolved references intact). `Cribrum.Elaborate` gained an
> `isInvisibleBlock` filter so RefDef / FootnoteDef blocks contribute
> no rendered output (the previous placeholder `<!-- comment -->`
> would have broken byte-equal conformance against the reference
> renderer). **Djot reference-suite gate** (`make djotref`): new
> `tools/run-djotref/` walks `test/djot-ref/corpus/*.test` files
> (one test per file, `=== input ===` / `=== expected ===` markers),
> runs each through Cribrum's parse + elaborate + render pipeline,
> normalises whitespace + strips the `<main>` wrapper, and compares
> against the expected reference output. A baseline file
> (`test/djot-ref/baseline.txt`) lists currently-passing tests;
> regressions fail the gate, additions are tracked. **101 / 246**
> upstream-ingested tests now pass at the baseline (full `jgm/djot`
> corpus ingestion shipped). **Step 11 parser remainder** (P5.4
> remainder): tight ul/ol list collapse (single-`Paragraph` items
> drop their `<p>` wrap in `<li>` for tight lists, matching the
> reference renderer); footnote refs (`[^label]` parses to
> `InlFootnoteRef`) + footnote-def blocks (`[^label]:` opener +
> indented continuation lines emit `FootnoteDef`); block-level
> attribute prefixes (`{#id .cls key=val}` on a line of its own
> attach Attrs to the following block, with consecutive prefixes
> stacking via `mergeAttrs` — id/key=val take last value, classes
> append). Elaborator gained `attrsToHAttrs` so Attrs reach the
> rendered HTML attribute list (class first, id second, key/val
> last-wins in source order). Thematic-break detection relaxed to
> accept internal whitespace (`- - - - -` is now `<hr>` per Djot —
> previously a one-item list). Emphasis / strong flank rule now
> matches Djot: marker pair is emphasised iff the opener and closer
> agree on inside-whitespace status (both `_ a _` and `_a_` are
> emphasis; asymmetric `_ a_` / `_a _` stay literal). Block-level
> Djot comments (`{% ... %}` spanning multiple lines) are
> recognised at the `groupLines` boundary and dropped.
> **Oracle corpus expansion** (P2.4):
> grew from 12 → 33 curated `(name, HExpr, expectedValid)` triples
> covering void elements, form controls under fieldset/legend,
> sectioning landmarks, details/summary, dl/dt/dd, ARIA labels +
> describedby, picture/source/img, time/mark, pre/code, progress/
> meter, blockquote-with-cite, nested lists, img with dims, and
> checkbox/radio inputs; 0 vnu-cross-check divergences. Backlog
> classes that exposed validator gaps (interactive-in-interactive,
> form-in-form, comment-in-script, orphan `<li>`/`<dt>`) are
> documented in the corpus comment for re-introduction once the
> matching validator slice lands.
>
> **Step 12 parser remainder** (P5.4 continued): inline verbatim
> spans now cross paragraph line breaks (`` `code\nwith a break` ``
> matches the Djot reference, corpus `verbatim-002`/`-007`);
> fenced divs `::: cls` parse to `Div` blocks (all 8 `fenced-divs-*`
> reference tests PASS — opener gated on a fresh block boundary so
> a mid-paragraph `::::` stays paragraph content; body collection
> tracks active code fences so `:::` lines inside ``` blocks stay
> inert); blockquote lazy continuation attaches unprefixed
> paragraph-continuable lines to the active blockquote (corpus
> `blockquote-002`/`-006`/`-007`/`-013`/`-014`); inline link +
> reference-definition URLs join across multi-line continuation
> by stripping internal whitespace (`[link](url\nandurl)` ->
> `href="urlandurl"`; `[label]:\n  url\n  andurl` -> RefDef body
> `"urlandurl"`). `TEAWeb.Sub` leaf install lands in
> `TEAWeb.Runtime` (FFI for keydown / `requestAnimationFrame` /
> `setInterval` / port slots) — Sub-driven deliveries route through
> the same `window.__cribrumDispatch` as view handlers, with each
> leaf's payload stashed into a dedicated window slot
> (`__cribrumKey` / `__cribrumTimestamp` / `__cribrumPortMsg`).
> `RuntimeState` gains a persistent `subHandlers` table so
> subscriptions survive view re-renders. Net djotref movement
> across the slice: 64 -> 101 / 246 (5x net gain).

> Snapshot of what's built. Authoritative narrative is `plan.dj`; the
> convention catalog is `docs/conventions.md`.

## Modules

| Module                    | Phase | Status |
|---------------------------|-------|--------|
| `Cribrum.Node`            | core  | HExpr IR (Element/Text/Comment, HAttr w/ handler-capable AttrValue, traversal). |
| `Cribrum.Djot.Surface`    | 1a    | Faithful Djot AST — full construct inventory representable. |
| `Cribrum.Djot.Parser`     | 1a    | **Slice**: paragraph, ATX heading (1-6), thematic break, block quote (recursive, with lazy paragraph continuation), fenced code block, fenced div (`::: cls` -> `Div` block with class attrs; opener requires fresh block boundary, body collection ignores code-fenced `:::`), unordered + ordered lists, pipe tables (with alignment row + header detection), inline emphasis (`_em_`), strong (`*strong*`), verbatim (`` `code` `` — spans paragraph line breaks), inline links (`[text](url)`; URL strips internal whitespace across line continuation), reference links (`[text][ref]` + collapsed `[text][]`), reference definitions (`[ref]: url ["title"]` with indented continuation lines and empty-URL form), inline images (`![alt](src)`), autolinks (`<url>` / `<email>` with non-empty + no-whitespace + has-`:`-or-`@` heuristic), hard breaks (trailing `\` on a non-last paragraph line), smart punctuation (`--`/`---`/`...` + orientation-aware curly quotes), task lists, definition lists, footnote refs/defs, block-level attribute prefixes (`{#id .cls key=val}`), block-level Djot comments. Top-level `parseDoc` is two-pass: raw parse, then walk-and-resolve `LinkReference` against the document's `RefDef` table. Inline tokenizer uses a plain-character accumulator so unpaired/empty markers fold back into the surrounding text without fragmenting `InlText` runs; multi-line paragraph bodies join with literal `'\n'` so verbatim spans naturally cross line breaks. |
| `Cribrum.Html.Category`   | 2     | Content-category enum (Metadata/Flow/Sectioning/Heading/Phrasing/Embedded/Interactive/Palpable/ScriptSupporting/FormAssociated). |
| `Cribrum.Html.Model`      | 2     | Public surface (types + lookups + attribute-name permission). Element catalog (114 elements) lives in `Cribrum.Html.Model.Generated`, ingested from `ingest/content-model.ts` by `ingest/html-model.ts` with `@webref/elements@2.6.0` cross-validation (invariant I8). `Cribrum.Html.Model.Types` carries `ElementSpec` / `ChildPolicy` (extracted to break the Model ↔ Generated cycle). `Cribrum.Html.Model.Invariants` lifts the TS-side tag-closure check (I2) to a decidable Idris proposition (`AllChildTagsExist` + `decAllChildTagsExist`); the test suite asserts the real catalog satisfies it. Drift gate `make ingest-check` fails on `@webref` bump, `content-model.ts` edit, or hand-edit to `Generated.idr` and pinpoints which input drifted. |
| `Cribrum.Html.Valid`      | 2     | `IsValidHtml = IsKnownTag × All AttrAllowedIn × All ChildAllowedIn × All IsValidHtml`. Total `decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`. Located rejection (`decideHtmlLocated`) returns path-into-tree + `RejectionClass` (UnknownTag / DisallowedAttr / IllegalChild / BlockInPhrasing / MalformedTable / TextNotAllowedIn / CommentNotAllowedIn). |
| `Cribrum.Elaborate`       | 1b    | Strict elaboration `Doc -> Either ElabError (h ** (IsValidHtml h, StructuralAA h))`. Phase-2 sharpened: failure path uses `LocatedHtmlError` carrying a `LocatedReject`. **Phase-4 sharpened**: `StructuralAA h` is now the actual conjunct of all 10 Phase-4 propositions (img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend, button-name, link-name, document-lang, heading-no-skip, duplicate-id) — `decStructuralAA` short-circuits to `StructuralAaFailure ruleId path` on the first failing predicate. AA failure-path **located**: per-node rules carry `Just path`; root-only `document-lang` carries `Just []`; whole-tree rules (heading-no-skip, duplicate-id) carry `Nothing`. |
| `Cribrum.Render.Html`     | 5     | Total `HExpr -> String`. HTML 5 void-element handling, escaping, handler attrs render as `data-on-<event>`. |
| `Cribrum.Render.Dom`      | 5     | **Spike**: tiny FFI surface (createElement/createTextNode/createComment/setAttribute/removeAttribute/addEventListener/appendChild/replaceChild/getElementById/clearChildren) + `currentEventValue` (input-value extraction) + `captureFocus`/`restoreFocus` (focus + selection-range preservation across reconcile). `renderDom : HExpr -> IO DomNode`, `reconcile` (Day-1 blow-and-rebuild, skip on unchanged tree, focus bracketed), `mountInto`. JS-backend only at runtime; chez type-checks via multi-spec %foreign with a scheme: fallback. Handler attrs dispatch via `window.__cribrumDispatch`; the dispatcher pre-extracts `event.target.value` into `window.__cribrumValue` for `onInput`/`onChange`. |
| `Cribrum.AA.Catalog`      | 3+4   | Shared rule catalog (single source of truth across pass + future types). Types (`Rule`/`Confidence`/`Severity`) in `Cribrum.AA.Catalog.Types`; rule data in `Cribrum.AA.Catalog.Generated`, ingested from `ingest/aa.ts` by `ingest/aa-catalog.ts` (plan §P3.1 scaffold) with `make ingest-check` drift gate. **19 rules** (Step 5 expansion): img-alt, anchor-href, alt-meaningful, heading-no-skip, document-lang, iframe-title, label-for-control, fieldset-legend, link-name, button-name, duplicate-id, unique-main, area-alt, link-empty-href, meta-no-refresh, summary-not-empty, track-kind (all Structural); aria-label-redundant, positive-tabindex (Heuristic). |
| `Cribrum.AA.Pass`         | 3     | **Data interpreter** (plan §P3.2): per-node traversal folds over `nodeRuleImpls : List (Rule, NodeRuleCheck)` — 16 rows pairing each catalog `Rule` with its impl through five tiny adapters (`pAttr`/`pNode`/`pCh`/`pAttrsOnly`/`pRoot`) that lift per-rule signatures to a single `Path -> Bool -> HExpr -> List Finding` shape. `treeRuleImpls` mirrors the same data view for the three whole-tree rules (heading-no-skip, duplicate-id, unique-main); `checkAA` keeps them as explicit `++` terms for mutation-gate stability. Total; confidence-partitioned (`structuralFindings` / `heuristicFindings`). Adding a per-node rule = one row. |
| `Cribrum.AA.Typed`        | 4     | **All 16 Structural rules** from the catalog promoted to type-level propositions via `So`: img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend, button-name, link-name, area-alt, link-empty-href, meta-no-refresh, summary-not-empty, track-kind (per-node `All` over `walkNodes`); document-lang (root-only); heading-no-skip, duplicate-id, unique-main (whole-tree bool + `So`). Decision via `decSo` (and `All` for per-node rules). Each per-node rule's typed wrapper is a 5-line alias through `Cribrum.AA.Promote`. Partitioning witness `isTypedPromoted : String -> Bool` exposes the promoted-id set; `Test.Cribrum.AA.Partition` (plan §P3.3) asserts it agrees with `confidence == Structural` across `allRules`. `Cribrum.Elaborate.StructuralAA` is the 16-tuple conjunct of these props; `decStructuralAA` short-circuits to `(ruleId, path)` on first failing predicate. |
| `Cribrum.AA.Promote`      | 4     | Per plan §P4.2 the bool-predicate + `So` + `All` over `walkNodes` pattern is factored into a single generic interface: `NodeOk pred`, `AllNodesOk pred`, `decAllNodesOk pred`, `allNodesOk pred`. Per-node rules in `Cribrum.AA.Typed` consume it; adding a new per-node rule is now ~5 lines. |
| `Cribrum.Pipeline.Anchor` | T6    | Heading-anchor pipeline pass. `slugify : String -> String` ASCII-folds + collapses runs of non-alphanumerics into single hyphens + trims edges. `addHeadingIds : HExpr -> HExpr` walks the tree pre-order and decorates every `<h1>..<h6>` lacking an `id` with `id="<slug>"`; duplicates disambiguate to `-2`/`-3`/... so produced trees never violate the `duplicate-id` AA rule. Idempotent. `harvestHeadings : HExpr -> List (Nat, String, String)` returns `(level, anchor, plainTitle)` rows in document order — consumed by `tools/render-docsnav/` to generate the TOC. |
| `TEAWeb.Html`             | T1    | **Spike**: view-builder smart constructors for 34 elements; `Attr msg` data type (`Plain` / `On`); `View msg` record = (HExpr, HandlerTable with `Event -> IO msg` closures); leaf nodes (`text_`, `comment_`); void elements (`br_`, `hr_`); plain attribute helpers (`class_`, `id_`, `href_`, ...). `viewSafe` routes through Phase-2's `decideHtmlLocated`, returning `Either ViewError ((h ** IsValidHtml h), HandlerTable)` with `LocatedReject` on rejection — content-model + attribute-permission misuse is caught dynamically with path-into-tree diagnostics. The dynamic gate now coexists with the typed-by-construction surface in `TEAWeb.Html.Typed`. `eventTargetValue` re-exports the value-extraction primitive from `Cribrum.Render.Dom`. |
| `TEAWeb.Html.Typed`       | T1    | **Phase-2 typed-by-construction view-builder** (plan §T1 follow-on). 30 element constructors (`pT_`/`ulT_`/`liT_`/...) return `TypedView <tag> msg`; child positions consume `Child <parent> msg` (`c_`/`tx_`/`cm_`) carrying a compile-time `So (isTagAllowedIn parent child)` witness. Catalog parity gated by `pddt_typed_predicate_matches_catalog` — 34 parents × 25 candidate child tags cross-validated against `Cribrum.Html.Valid.childAllowedBool`. Bridges to the untyped layer via `unTyped : TypedView tag msg -> View msg` for callers (e.g. `TEAWeb.Program.view`). |
| `TEAWeb.Event`            | T2    | **Spike**: `onClick`/`onSubmit`/`onFocus`/`onBlur`/`onDoubleClick`/`onMouseEnter`/`onMouseLeave` (msg-form, IO-wrapped via `pure`); `onInput`/`onChange` (String-callback form; the closure runs `eventTargetValue` to read the pre-extracted `event.target.value`); `onKeyDown`/`onKeyUp` (String-callback form; closure reads `event.key` via `eventKey`/`currentEventKey`). Callback ids app-supplied for MVP; deterministic `hash(path, event)` when keyed diff lands. |
| `TEAWeb.Program`          | T3    | **Spike**: `Program model msg` record (init/update/view/subscriptions). |
| `TEAWeb.Cmd`              | T4    | **Spike**: `None`/`Batch`/`Focus`/`Blur`. `flatten : Cmd msg -> List (Cmd msg)`. Http/Random/After deferred to TEAWeb T6 demo's needs. |
| `TEAWeb.Sub`              | T4    | **Leaves shipped**: `None`/`Batch` + `OnKeyDown` / `OnAnimationFrame` / `Every` / `Port`. `flatten` strips `None` / `Batch` to leaf list; `subCallbackId` exposes per-leaf cbId. `OnResize` deferred. Runtime install lives in `TEAWeb.Runtime`. |
| `TEAWeb.Runtime`          | T3+T4 | **MVP runtime + Sub-leaf install**: `mount` + tail-recursive interpreter loop in Idris; `installDispatch` installs single global `window.__cribrumDispatch`; `runCmd` interprets Focus/Blur via FFI; reconcile after each update keeps state ref + handler table in lockstep. `installSubs` walks the initial `Sub msg` tree at mount, calls the right installer per leaf (`installSubKeyDown` / `installSubAnimationFrame` / `installSubInterval` / `installSubPort`), and stashes a `(cbId, Event -> IO msg)` projection that reads the right window slot (`__cribrumKey` / `__cribrumTimestamp` / `__cribrumPortMsg`). `RuntimeState.subHandlers` persists across renders so Sub-driven deliveries survive view-handler refresh. Full sub-tree diff between renders deferred to keyed-children reconcile. JS-backend execution only; chez type-checks via multi-spec %foreign with a scheme: fallback. |
| `TEAWeb.Ports`            | T5    | Not started. Typed JSON FFI boundary for app-specific JS. |

## Tests (512 total, all green)

```
$ make test-fast        # cribrum + teaweb suites
$ make test             # adds ingest drift gate + mutation gate
```

Counts per group: 18 Node + 16 Surface + 128 Parser + 20 Model + 52 Valid
+ 31 Elaborate + 17 Render.Html + 1 Render.Dom + 56 AA.Pass + 88 AA.Typed
+ 5 AA.Partition + 16 Pipeline.Anchor + 6 Integration (real README.dj +
plan.dj read at test time, plus pipeline determinism check) = 454 Cribrum.
10 TEAWeb.Html + 9 TEAWeb.Html.Typed + 10 TEAWeb.Event + 6 TEAWeb.Cmd
+ 14 TEAWeb.Sub + 9 TEAWeb.Program = 58 TEAWeb. Plus the djot-ref
reference-suite gate (`tools/run-djotref/`, 246 corpus tests against a
101-test baseline; separate harness from the in-suite tests).
**Total: 512 in-suite tests across 19 groups, plus 246 djot-ref tests
(101 passing, 145 expected-fail under baseline).**

Each module has:
- **EXTs** (example tests) — canonical cases for each behaviour.
- **PDDTs** (parameterised data-driven tests) — tables sweeping the input
  space.
- **PBTs** (property-based tests via hedgehog) — invariants over generated
  inputs.

## Mutation gate (209 mutants, 0 surviving)

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
  smart constructors **shipped in `TEAWeb.Html.Typed`** (plan §T1
  post-Phase-2 follow-on): typed `Child` positions carry compile-time
  catalog witnesses, drift-gated against `childAllowedBool`. The
  dynamic `viewSafe` gate in `TEAWeb.Html` remains as a safety net for
  views built outside the typed surface.
- **Phase 3 catalog**: 12 of ~50 WCAG AA success criteria currently
  modelled (img-alt, anchor-href, alt-meaningful, heading-no-skip,
  document-lang, iframe-title, label-for-control, fieldset-legend,
  link-name, button-name, duplicate-id, unique-main). Plan calls for
  ACT-rule ingestion to drive the full catalog; `ingest/aa.ts` is the
  scaffold target. Pass implementation is still per-rule hand-
  written; data-interpreter refactor (plan §P3.2) tracked separately.
- **Phase 4**: all 11 Structural AA rules in the catalog are now promoted
  to types (img-alt, anchor-href, iframe-title, label-for-control,
  fieldset-legend, button-name, link-name, document-lang, heading-no-
  skip, duplicate-id, unique-main) AND the `Cribrum.AA.Promote` factor
  (plan §P4.2)
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
  scaffold (12 rules, `Cribrum.AA.Catalog.Generated` from `ingest/aa.ts`
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
