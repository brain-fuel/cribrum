||| Partitioning audit (plan.dj §P3.3): every Structural rule in the
||| catalog has a Phase-4 typed promotion; no Heuristic/Runtime rule
||| does. Closes the loop now that `Cribrum.AA.Catalog` is single-
||| sourced from `ingest/aa.ts` and `Cribrum.AA.Typed` exposes
||| `isTypedPromoted` as the partition witness.
module Test.Cribrum.AA.Partition

import Data.List
import Hedgehog
import Cribrum.AA.Catalog
import Cribrum.AA.Typed

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

-- Helpers.

ruleIsStructural : Rule -> Bool
ruleIsStructural r = confidence r == Structural

structuralRules : List Rule
structuralRules = filter ruleIsStructural allRules

nonStructuralRules : List Rule
nonStructuralRules = filter (not . ruleIsStructural) allRules

structuralIds : List String
structuralIds = map id structuralRules

nonStructuralIds : List String
nonStructuralIds = map id nonStructuralRules

promotedIdsInCatalog : List String
promotedIdsInCatalog = filter (\r => isTypedPromoted (id r)) allRules >>= \r => [id r]

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

||| Every Structural rule in the catalog has a typed promotion.
export
ext_every_structural_promoted : Property
ext_every_structural_promoted = oneShot $
  all isTypedPromoted structuralIds === True

||| No Heuristic/Runtime rule has a typed promotion.
export
ext_no_nonstructural_promoted : Property
ext_no_nonstructural_promoted = oneShot $
  any isTypedPromoted nonStructuralIds === False

||| Promoted-id count exactly matches the Structural rule count —
||| catches "we promoted a rule but its id is not in the catalog yet"
||| (or vice versa).
export
ext_promoted_count_matches : Property
ext_promoted_count_matches = oneShot $
  length promotedIdsInCatalog === length structuralIds

||| The 10 expected Structural rule ids — pins the catalog shape and
||| guards against silent renames in `aa.ts`.
export
ext_structural_ids_canonical : Property
ext_structural_ids_canonical = oneShot $
  sort structuralIds ===
    sort [ "img-alt"
         , "anchor-href"
         , "iframe-title"
         , "label-for-control"
         , "fieldset-legend"
         , "button-name"
         , "link-name"
         , "document-lang"
         , "heading-no-skip"
         , "duplicate-id"
         , "unique-main"
         ]

||| `alt-meaningful` (Heuristic) is the lone non-Structural rule today.
||| Pinned so any reclassification (or addition of a new Heuristic /
||| Runtime rule) lands in the audit explicitly.
export
ext_nonstructural_ids_canonical : Property
ext_nonstructural_ids_canonical = oneShot $
  sort nonStructuralIds === sort ["alt-meaningful"]

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "Cribrum.AA.Partition"
  [ ("ext_every_structural_promoted",     ext_every_structural_promoted)
  , ("ext_no_nonstructural_promoted",     ext_no_nonstructural_promoted)
  , ("ext_promoted_count_matches",        ext_promoted_count_matches)
  , ("ext_structural_ids_canonical",      ext_structural_ids_canonical)
  , ("ext_nonstructural_ids_canonical",   ext_nonstructural_ids_canonical)
  ]
