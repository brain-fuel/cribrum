||| Tests for `Cribrum.Pipeline.Anchor` — autoId, addSectionIds,
||| harvestHeadings. EXT (canonical cases), PDDT (table over id
||| inputs), PBT (idempotence of addSectionIds).
module Test.Cribrum.Pipeline.Anchor

import Data.List
import Data.String
import Hedgehog
import Cribrum.Node
import Cribrum.Pipeline.Anchor

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- autoId EXTs (Djot reference identifier: case-preserving, `_` kept,
-- runs of other chars -> single hyphen, edges trimmed).
--------------------------------------------------------------------------------

export
ext_autoId_simple : Property
ext_autoId_simple = oneShot $ autoId "Phase order" === "Phase-order"

export
ext_autoId_preserves_case : Property
ext_autoId_preserves_case = oneShot $ autoId "ABC" === "ABC"

export
ext_autoId_collapses_runs : Property
ext_autoId_collapses_runs = oneShot $
  autoId "Phase 1 — Djot parser + elaborate"
    === "Phase-1-Djot-parser-elaborate"

export
ext_autoId_trims_edges : Property
ext_autoId_trims_edges = oneShot $
  autoId "  --- hello --- " === "hello"

export
ext_autoId_empty : Property
ext_autoId_empty = oneShot $ autoId "" === ""

export
ext_autoId_only_punct : Property
ext_autoId_only_punct = oneShot $ autoId " — — " === ""

export
ext_autoId_idempotent : Property
ext_autoId_idempotent = oneShot $
  autoId (autoId "What Cribrum is") === autoId "What Cribrum is"

--------------------------------------------------------------------------------
-- autoId PDDT.
--------------------------------------------------------------------------------

idCases : List (String, String)
idCases =
  [ ("Cribrum",                                  "Cribrum")
  , ("What Cribrum is",                          "What-Cribrum-is")
  , ("Current status",                           "Current-status")
  , ("The IR (single-tree HExpr)",               "The-IR-single-tree-HExpr")
  , ("HTML validity as a refinement",            "HTML-validity-as-a-refinement")
  , ("Phase 1 — Djot parser + elaborate",        "Phase-1-Djot-parser-elaborate")
  , ("TEAWeb — Elm++ on Cribrum",                "TEAWeb-Elm-on-Cribrum")
  , ("TEAWeb typed-by-content view API",         "TEAWeb-typed-by-content-view-API")
  , ("Convention layer",                         "Convention-layer")
  , ("Ingest pipeline",                          "Ingest-pipeline")
  ]

export
pddt_autoId_table : Property
pddt_autoId_table = withTests 1 . property $ do
  for_ idCases $ \(input, expected) =>
    autoId input === expected

--------------------------------------------------------------------------------
-- addSectionIds EXTs.
--------------------------------------------------------------------------------

sectionH : String -> String -> HExpr
sectionH tag t = Element "section" [] [Element tag [] [Text t]]

idOf : List HAttr -> Maybe String
idOf []                                = Nothing
idOf (MkHAttr "id" (Str s) :: _)       = Just s
idOf (_ :: rest)                       = idOf rest

||| `<section><h2>What Cribrum is</h2></section>` -> section gets
||| id="What-Cribrum-is"; the heading itself stays id-less.
export
ext_addIds_assigns_id_to_section : Property
ext_addIds_assigns_id_to_section = oneShot $
  let anchored = addSectionIds (sectionH "h2" "What Cribrum is")
   in case anchored of
        Element "section" sAttrs [Element "h2" hAttrs _] => do
          idOf sAttrs === Just "What-Cribrum-is"
          idOf hAttrs === Nothing
        _ => failWith Nothing "shape changed unexpectedly"

||| Existing `id` on a section is preserved (no overwrite).
export
ext_addIds_preserves_existing : Property
ext_addIds_preserves_existing = oneShot $
  let tree     = Element "section" [MkHAttr "id" (Str "custom")]
                   [Element "h2" [] [Text "Anything"]]
      anchored = addSectionIds tree
   in case anchored of
        Element "section" attrs _ => idOf attrs === Just "custom"
        _ => failWith Nothing "shape changed unexpectedly"

||| Non-section elements get no id.
export
ext_addIds_skips_non_section : Property
ext_addIds_skips_non_section = oneShot $
  let anchored = addSectionIds (Element "p" [] [Text "no id here"])
   in case anchored of
        Element "p" attrs _ => idOf attrs === Nothing
        _ => failWith Nothing "shape changed unexpectedly"

||| Two sibling sections with identical heading text get disambiguated:
||| first gets the base id; second gets `-1` (Djot reference scheme).
export
ext_addIds_disambiguates_duplicates : Property
ext_addIds_disambiguates_duplicates = oneShot $
  let tree     = Element "main" []
                   [ sectionH "h2" "Phase"
                   , sectionH "h2" "Phase"
                   ]
      anchored = addSectionIds tree
   in case anchored of
        Element "main" _ [Element "section" a1 _, Element "section" a2 _] => do
          idOf a1 === Just "Phase"
          idOf a2 === Just "Phase-1"
        _ => failWith Nothing "shape changed unexpectedly"

||| A nested section disambiguates against its ancestor's just-assigned
||| id: outer `Foo`, inner `Foo-1`.
export
ext_addIds_nested_disambiguates : Property
ext_addIds_nested_disambiguates = oneShot $
  let tree     = Element "section" []
                   [ Element "h1" [] [Text "Foo"]
                   , Element "section" [] [Element "h2" [] [Text "Foo"]]
                   ]
      anchored = addSectionIds tree
   in case anchored of
        Element "section" outer (_ :: Element "section" inner _ :: _) => do
          idOf outer === Just "Foo"
          idOf inner === Just "Foo-1"
        _ => failWith Nothing "shape changed unexpectedly"

||| `addSectionIds . addSectionIds = addSectionIds`.
export
ext_addIds_idempotent : Property
ext_addIds_idempotent = oneShot $
  let tree    = Element "main" []
                  [ sectionH "h2" "Alpha"
                  , sectionH "h3" "Beta"
                  ]
      once    = addSectionIds tree
      twice   = addSectionIds once
   in twice === once

--------------------------------------------------------------------------------
-- harvestHeadings EXTs.
--------------------------------------------------------------------------------

||| Document order: pre-order walk; anchors from the section `id`,
||| level + title from the section's heading.
export
ext_harvest_in_document_order : Property
ext_harvest_in_document_order = oneShot $
  let tree = Element "main" []
               [ Element "section" [MkHAttr "id" (Str "title")]
                   [ Element "h1" [] [Text "Title"]
                   , Element "section" [MkHAttr "id" (Str "first")]
                       [ Element "h2" [] [Text "First"]
                       , Element "section" [MkHAttr "id" (Str "first-a")]
                           [Element "h3" [] [Text "First.A"]]
                       ]
                   ]
               , Element "section" [MkHAttr "id" (Str "second")]
                   [Element "h1" [] [Text "Second"]]
               ]
      rows = harvestHeadings tree
   in rows === [ (1, "title",   "Title")
               , (2, "first",   "First")
               , (3, "first-a", "First.A")
               , (1, "second",  "Second")
               ]

||| harvestHeadings on a tree with no sections is empty.
export
ext_harvest_no_sections : Property
ext_harvest_no_sections = oneShot $
  harvestHeadings (Element "p" [] [Text "no sections"]) === []

||| addSectionIds + harvestHeadings round-trip: harvested anchors match
||| the section ids the same pass wrote (case-preserving).
export
ext_harvest_after_anchor_roundtrip : Property
ext_harvest_after_anchor_roundtrip = oneShot $
  let tree     = Element "main" []
                   [ sectionH "h2" "Foo Bar"
                   , sectionH "h3" "Baz"
                   ]
      anchored = addSectionIds tree
      rows     = harvestHeadings anchored
   in rows === [ (2, "Foo-Bar", "Foo Bar")
               , (3, "Baz",     "Baz")
               ]

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "Cribrum.Pipeline.Anchor"
  [ ("ext_autoId_simple",                   ext_autoId_simple)
  , ("ext_autoId_preserves_case",           ext_autoId_preserves_case)
  , ("ext_autoId_collapses_runs",           ext_autoId_collapses_runs)
  , ("ext_autoId_trims_edges",              ext_autoId_trims_edges)
  , ("ext_autoId_empty",                    ext_autoId_empty)
  , ("ext_autoId_only_punct",               ext_autoId_only_punct)
  , ("ext_autoId_idempotent",               ext_autoId_idempotent)
  , ("pddt_autoId_table",                   pddt_autoId_table)
  , ("ext_addIds_assigns_id_to_section",    ext_addIds_assigns_id_to_section)
  , ("ext_addIds_preserves_existing",       ext_addIds_preserves_existing)
  , ("ext_addIds_skips_non_section",        ext_addIds_skips_non_section)
  , ("ext_addIds_disambiguates_duplicates", ext_addIds_disambiguates_duplicates)
  , ("ext_addIds_nested_disambiguates",     ext_addIds_nested_disambiguates)
  , ("ext_addIds_idempotent",               ext_addIds_idempotent)
  , ("ext_harvest_in_document_order",       ext_harvest_in_document_order)
  , ("ext_harvest_no_sections",             ext_harvest_no_sections)
  , ("ext_harvest_after_anchor_roundtrip",  ext_harvest_after_anchor_roundtrip)
  ]
