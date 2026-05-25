||| WCAG AA rule catalog — Phase 3 + Phase 4 *shared* source of truth.
|||
||| Per plan.dj §Phase 4: "One shared rule catalog across Phase 1b (elaboration
||| enforcement), Phase 3 (pass), and Phase 4 (types). A rule 'graduates' from
||| finding to proposition; it is never defined twice."
|||
||| This module is the public surface. The types (`Rule`, `Confidence`,
||| `Severity`) live in `Cribrum.AA.Catalog.Types`; the rule data lives in
||| `Cribrum.AA.Catalog.Generated`, ingested from `ingest/aa.ts` by
||| `ingest/aa-catalog.ts` (plan §P3.1). Both are re-exported here so existing
||| call sites (`Cribrum.AA.Pass`, `Cribrum.AA.Typed`, `Cribrum.AA.Promote`,
||| `Cribrum.Elaborate`) keep their single import.
|||
||| Drift gate: `make ingest-check` runs `aa-catalog.ts --check` alongside
||| `html-model.ts --check`; hand-edits to `Generated.idr` fail the gate.
module Cribrum.AA.Catalog

import public Cribrum.AA.Catalog.Types
import public Cribrum.AA.Catalog.Generated

%default total

||| All rules in the catalog. Aliased to the generated list so the data
||| source-of-truth (`ingest/aa.ts`) stays single-source.
public export
allRules : List Rule
allRules = generatedRules
