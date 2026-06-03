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

||| The expected Structural rule ids — pins the catalog shape and
||| guards against silent renames in `aa.ts`. 27 Structural rules:
||| 16 prior (11 originals + area-alt, link-empty-href, meta-no-refresh,
||| summary-not-empty, track-kind) + input-image-alt, object-name,
||| th-scope-valid, th-has-name, no-empty-heading + select-has-options,
||| caption-first-child, input-button-name, aria-hidden-body,
||| aria-role-valid, autocomplete-valid.
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
         , "area-alt"
         , "link-empty-href"
         , "meta-no-refresh"
         , "summary-not-empty"
         , "track-kind"
         , "input-image-alt"
         , "object-name"
         , "th-scope-valid"
         , "th-has-name"
         , "no-empty-heading"
         , "select-has-options"
         , "caption-first-child"
         , "input-button-name"
         , "aria-hidden-body"
         , "aria-role-valid"
         , "autocomplete-valid"
         ]

||| Heuristic/Runtime rules. Pinned so any reclassification (or addition
||| of a new Heuristic / Runtime rule) lands in the audit explicitly.
|||
||| Three hand-curated Cribrum heuristics (`alt-meaningful`,
||| `aria-label-redundant`, `positive-tabindex`); the §P3.2 Runtime
||| catalog-metadata rows for the WCAG A/AA success criteria that are
||| real but NOT statically decidable from a static HExpr (contrast,
||| reflow, keyboard, focus-visible, error handling, status messages,
||| captions/audio-description, etc.) — these document the full AA
||| landscape and the audit proves they stay out of the type layer; and
||| the ACT-rules ingest corpus (`act-*`, plan §P3.1/§P3.2). The ACT rows
||| land Heuristic because their expectations are accessible-name /
||| accessibility-tree predicates not yet statically decidable on
||| Cribrum's HTML tree alone; a follow-up promotes the tree-decidable
||| ones to Structural.
export
ext_nonstructural_ids_canonical : Property
ext_nonstructural_ids_canonical = oneShot $
  sort nonStructuralIds ===
    sort [ "alt-meaningful"
         , "aria-label-redundant"
         , "positive-tabindex"
         -- Runtime catalog-metadata rows (plan §P3.2): undecidable WCAG A/AA.
         , "audio-description"
         , "captions-live"
         , "character-key-shortcuts"
         , "consistent-identification"
         , "consistent-navigation"
         , "content-on-hover-focus"
         , "contrast-minimum"
         , "error-identification"
         , "error-prevention-legal"
         , "error-suggestion"
         , "focus-visible"
         , "images-of-text"
         , "keyboard"
         , "label-in-name"
         , "motion-actuation"
         , "no-keyboard-trap"
         , "non-text-contrast"
         , "orientation"
         , "pointer-cancellation"
         , "pointer-gestures"
         , "reflow"
         , "resize-text"
         , "status-messages"
         , "text-spacing"
         -- ACT-rules ingest corpus (Heuristic).
         , "act-aria-state-or-property-has-valid-value-6a7281"
         , "act-attribute-is-not-duplicated-e6952f"
         , "act-autocomplete-attribute-has-valid-value-73f2c2"
         , "act-button-has-non-empty-accessible-name-97a4e1"
         , "act-form-field-has-non-empty-accessible-name-e086e5"
         , "act-html-page-has-lang-attribute-b5c3f8"
         , "act-html-page-has-non-empty-title-2779a5"
         , "act-id-attribute-value-is-unique-3ea0c8"
         , "act-iframe-element-has-non-empty-accessible-name-cae760"
         , "act-image-button-has-non-empty-accessible-name-59796f"
         , "act-image-has-non-empty-accessible-name-23a2a8"
         , "act-link-has-non-empty-accessible-name-c487ae"
         , "act-meta-viewport-allows-for-zoom-b4f0c3"
         , "act-object-element-rendering-non-text-content-has-non-empty-accessible-name-8fc3b6"
         , "act-role-attribute-has-valid-value-674b10"
         , "act-summary-element-has-non-empty-accessible-name-2t702h"
         ]

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
