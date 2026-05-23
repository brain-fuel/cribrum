# Cribrum

> A single intermediate representation — the **HExpr** — authored as prose
> (Djot), elaborated into full semantic, accessible HTML, typechecked as HTML,
> and checked for WCAG AA accessibility. All conformance is expressed as
> nested refinements over **one** node type, never a family of separate types.

**Language:** Idris 2 (core IR, elaboration, type-level conformance, decision
procedures). JS via the Idris 2 JS backend for the final DOM render.

See `plan.md` for the architectural narrative, `STATUS.md` for what currently
ships, and `docs/conventions.md` for the convention catalog.

## Build + run

```
$ pack install cribrum            # build the library
$ pack run test/test.ipkg         # run the test suite
$ test/mutation/run.sh            # mutation gate
```

## Project layout

```
src/Cribrum/
  Node.idr               # HExpr — the IR
  Djot/Surface.idr       # faithful Djot surface AST
  Djot/Parser.idr        # native Djot parser
  Elaborate.idr          # surface -> (HExpr ** IsValidHtml × StructuralAA)
  Html/Valid.idr         # IsValidHtml + decideHtml
  AA/Catalog.idr         # shared WCAG AA rule catalog
  AA/Pass.idr            # checkAA — the pass-style checker
  AA/Typed.idr           # img-alt as a type-level proposition (Phase 4)
  Render/Html.idr        # HExpr -> String

test/
  src/Test/Cribrum/...   # one test module per src module
  src/Main.idr           # hedgehog test entry
  mutation/              # mutants.tsv + run.sh

docs/
  conventions.md         # the Djot/class -> semantic-HTML convention catalog

README.dj                # dogfood — project's own README in Djot
README.md                # this file
plan.md                  # authoritative architectural narrative
STATUS.md                # snapshot of phase progress
```

## Testing discipline (per `plan.md` cross-cutting commitments)

Every module ships:

- **EXTs** — example tests, single canonical cases.
- **PDDTs** — parameterised data-driven tests, tables sweeping the input space.
- **PBTs** — property-based tests via hedgehog, invariants over generated inputs.

Plus a project-wide **mutation gate**: `test/mutation/run.sh` reads
`test/mutation/mutants.tsv`, applies each mutant, runs the test suite, and
fails the gate if any non-approved mutant survives. Approved survivors are
listed in the `APPROVED` env var (currently empty — zero approved survivors).
