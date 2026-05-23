# Cribrum — Current Status

> Snapshot of what's built. Authoritative narrative is `plan.md`; the
> convention catalog is `docs/conventions.md`.

## Modules

| Module                    | Phase | Status |
|---------------------------|-------|--------|
| `Cribrum.Node`            | core  | HExpr IR (Element/Text/Comment, HAttr w/ handler-capable AttrValue, traversal). |
| `Cribrum.Djot.Surface`    | 1a    | Faithful Djot AST — full construct inventory representable. |
| `Cribrum.Djot.Parser`     | 1a    | **Slice**: paragraph, ATX heading (1-6), thematic break. Code block, block quote, lists, inline emphasis parsing → deferred. |
| `Cribrum.Html.Valid`      | 2     | **Spike**: IsKnownTag membership only; per-element content model + attribute permission deferred. Indexed proposition + total Dec works. |
| `Cribrum.Elaborate`       | 1b    | Strict elaboration `Doc -> Either ElabError (h ** (IsValidHtml h, StructuralAA h))`. StructuralAA is unit in this spike. |
| `Cribrum.Render.Html`     | 5     | Total `HExpr -> String`. HTML 5 void-element handling, escaping, handler attrs render as `data-on-<event>`. DOM render → deferred. |
| `Cribrum.AA.Catalog`      | 3+4   | Shared rule catalog (single source of truth across pass + future types). |
| `Cribrum.AA.Pass`         | 3     | **Spike**: img-alt, anchor-href, alt-meaningful, heading-no-skip. Total. Confidence-partitioned (Structural / Heuristic). |

## Tests (129 total, all green)

```
$ pack run test/test.ipkg
```

Counts per group: 18 Node + 16 Surface + 33 Parser + 14 Valid + 15 Elaborate
+ 17 Render + 14 AA + 2 Integration.

Each module has:
- **EXTs** (example tests) — canonical cases for each behaviour.
- **PDDTs** (parameterised data-driven tests) — tables sweeping the input
  space.
- **PBTs** (property-based tests via hedgehog) — invariants over generated
  inputs.

## Mutation gate (53 mutants, 0 surviving)

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

The integration test (`Test.Cribrum.Integration`) runs `README.dj` through
the whole pipeline and asserts:
- Every heading + thematic break + `<main>` landmark survives.
- No bare top-level `<div>` (the "no div/span soup" commitment).
- The dependent pair carries a real `IsValidHtml h` witness.

## What's NOT shipped yet

- **Phase 1a parser**: code blocks, block quotes, lists, tables, inline
  emphasis/links/images/footnotes parsing, generic attribute block parsing,
  reference link definitions, raw blocks, smart-punctuation tokenisation.
- **Phase 2 content model**: per-element permitted children, per-element
  permitted attributes, ingestion from HTML spec.
- **Phase 3 catalog**: full WCAG AA rule set (currently 4 of ~50 success
  criteria).
- **Phase 4**: type-level promotion of structural AA rules.
- **Phase 5 DOM render**: only the string-form renderer ships; the
  Idris-2-JS-backend DOM bridge is future work.
- **Convention layer**: §2 of `docs/conventions.md` is mostly deferred —
  elaboration currently only wraps blocks in `<main>`; `:::nav` etc. don't
  promote yet.
- **Ingest pipeline**: HTML content model + WCAG AA catalog are
  hand-listed (the spike subsets) rather than ingested from W3C upstreams.
