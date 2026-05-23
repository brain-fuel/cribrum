# Cribrum — Current Status

> Snapshot of what's built. Authoritative narrative is `plan.dj`; the
> convention catalog is `docs/conventions.md`.

## Modules

| Module                    | Phase | Status |
|---------------------------|-------|--------|
| `Cribrum.Node`            | core  | HExpr IR (Element/Text/Comment, HAttr w/ handler-capable AttrValue, traversal). |
| `Cribrum.Djot.Surface`    | 1a    | Faithful Djot AST — full construct inventory representable. |
| `Cribrum.Djot.Parser`     | 1a    | **Slice**: paragraph, ATX heading (1-6), thematic break, block quote (recursive), fenced code block. Lists, inline emphasis parsing, tables → deferred. |
| `Cribrum.Html.Valid`      | 2     | **Spike**: IsKnownTag membership only; per-element content model + attribute permission deferred. Indexed proposition + total Dec works. |
| `Cribrum.Elaborate`       | 1b    | Strict elaboration `Doc -> Either ElabError (h ** (IsValidHtml h, StructuralAA h))`. StructuralAA is unit in this spike. |
| `Cribrum.Render.Html`     | 5     | Total `HExpr -> String`. HTML 5 void-element handling, escaping, handler attrs render as `data-on-<event>`. DOM render → deferred. |
| `Cribrum.AA.Catalog`      | 3+4   | Shared rule catalog (single source of truth across pass + future types). |
| `Cribrum.AA.Pass`         | 3     | **Spike**: img-alt, anchor-href, alt-meaningful, heading-no-skip. Total. Confidence-partitioned (Structural / Heuristic). |
| `Cribrum.AA.Typed`        | 4     | **Spike**: img-alt and anchor-href promoted to type-level propositions via `So`. Decision via `decSo` + `All` over walked nodes. Each rule wires in independently. |
| `TEAWeb.Html`             | T1    | Not started. View-builder smart constructors (`div_`, `button_`, …) returning HExpr; pre-Phase-2 uses dynamic `viewSafe` gate. |
| `TEAWeb.Event`            | T2    | Not started. `onClick`/`onInput`/… handlers; `View msg` wrapper + `HandlerTable msg`; callback ids = `hash(path, event)`. |
| `TEAWeb.Program`          | T3    | Not started. `Program model msg` record + `mount`; tail-recursive interpreter loop in Idris. |
| `TEAWeb.Cmd`              | T4    | Not started. `Cmd msg` variants (Http, Focus, Blur, Random, After). |
| `TEAWeb.Sub`              | T4    | Not started. `Sub msg` variants (OnAnimationFrame, OnKeyDown, OnResize, Every, Port). |
| `TEAWeb.Runtime`          | T3+T4 | Not started. Effect dispatcher; the only consumer of `Cribrum.Render.Dom`. |
| `TEAWeb.Ports`            | T5    | Not started. Typed JSON FFI boundary for app-specific JS. |

## Tests (175 total, all green)

```
$ pack run test/test.ipkg
```

Counts per group: 18 Node + 16 Surface + 57 Parser + 14 Valid + 15 Elaborate
+ 17 Render + 14 AA.Pass + 20 AA.Typed + 4 Integration (README.dj + plan.dj).

Each module has:
- **EXTs** (example tests) — canonical cases for each behaviour.
- **PDDTs** (parameterised data-driven tests) — tables sweeping the input
  space.
- **PBTs** (property-based tests via hedgehog) — invariants over generated
  inputs.

## Mutation gate (74 mutants, 0 surviving)

```
$ test/mutation/run.sh
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
- **Phase 2 content model**: per-element permitted children, per-element
  permitted attributes, ingestion from HTML spec.
- **Phase 3 catalog**: full WCAG AA rule set (currently 4 of ~50 success
  criteria).
- **Phase 4**: the remaining structural AA rules (heading-no-skip and the
  deferred ones in `docs/conventions.md` §4). Pattern is locked by the two
  rules already promoted.
- **Phase 5 DOM render**: only the string-form renderer ships; the
  Idris-2-JS-backend DOM bridge is future work.
- **Convention layer**: §2 of `docs/conventions.md` is mostly deferred —
  elaboration currently only wraps blocks in `<main>`; `:::nav` etc. don't
  promote yet.
- **Ingest pipeline**: HTML content model + WCAG AA catalog are
  hand-listed (the spike subsets) rather than ingested from W3C upstreams.
- **Phase T (TEAWeb)**: entire layer not started — no view-builder, no
  event model, no Program/mount, no Cmd/Sub interpreter, no Ports. Blocks
  on Phase 5 DOM bridge for the MVP demo.
