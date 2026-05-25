||| Tests for `Cribrum.Pipeline.Anchor` — slugify, addHeadingIds,
||| harvestHeadings. EXT (canonical cases), PDDT (table over slug
||| inputs), PBT (idempotence of addHeadingIds).
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
-- slugify EXTs.
--------------------------------------------------------------------------------

export
ext_slugify_simple : Property
ext_slugify_simple = oneShot $ slugify "Phase order" === "phase-order"

export
ext_slugify_lowercases : Property
ext_slugify_lowercases = oneShot $ slugify "ABC" === "abc"

export
ext_slugify_collapses_runs : Property
ext_slugify_collapses_runs = oneShot $
  slugify "Phase 1 — Djot parser + elaborate"
    === "phase-1-djot-parser-elaborate"

export
ext_slugify_trims_edges : Property
ext_slugify_trims_edges = oneShot $
  slugify "  --- hello --- " === "hello"

export
ext_slugify_empty : Property
ext_slugify_empty = oneShot $ slugify "" === ""

export
ext_slugify_only_punct : Property
ext_slugify_only_punct = oneShot $ slugify " — — " === ""

export
ext_slugify_idempotent : Property
ext_slugify_idempotent = oneShot $
  slugify (slugify "What Cribrum is") === slugify "What Cribrum is"

--------------------------------------------------------------------------------
-- slugify PDDT.
--------------------------------------------------------------------------------

slugCases : List (String, String)
slugCases =
  [ ("Cribrum",                                  "cribrum")
  , ("What Cribrum is",                          "what-cribrum-is")
  , ("Current status",                           "current-status")
  , ("The IR (single-tree HExpr)",               "the-ir-single-tree-hexpr")
  , ("HTML validity as a refinement",            "html-validity-as-a-refinement")
  , ("Phase 1 — Djot parser + elaborate",        "phase-1-djot-parser-elaborate")
  , ("TEAWeb — Elm++ on Cribrum",                "teaweb-elm-on-cribrum")
  , ("TEAWeb typed-by-content view API",         "teaweb-typed-by-content-view-api")
  , ("Convention layer",                         "convention-layer")
  , ("Ingest pipeline",                          "ingest-pipeline")
  ]

export
pddt_slugify_table : Property
pddt_slugify_table = withTests 1 . property $ do
  for_ slugCases $ \(input, expected) =>
    slugify input === expected

--------------------------------------------------------------------------------
-- addHeadingIds EXTs.
--------------------------------------------------------------------------------

hExpr_h2 : String -> HExpr
hExpr_h2 t = Element "h2" [] [Text t]

idOf : List HAttr -> Maybe String
idOf []                                = Nothing
idOf (MkHAttr "id" (Str s) :: _)       = Just s
idOf (_ :: rest)                       = idOf rest

||| `<h2>What Cribrum is</h2>` -> id="what-cribrum-is".
export
ext_addIds_assigns_slug_from_text : Property
ext_addIds_assigns_slug_from_text = oneShot $
  let tree     = Element "section" [] [hExpr_h2 "What Cribrum is"]
      anchored = addHeadingIds tree
   in case anchored of
        Element "section" _ [Element "h2" attrs _] =>
          idOf attrs === Just "what-cribrum-is"
        _ => failWith Nothing "shape changed unexpectedly"

||| Existing `id` on a heading is preserved (no overwrite).
export
ext_addIds_preserves_existing : Property
ext_addIds_preserves_existing = oneShot $
  let h        = Element "h2" [MkHAttr "id" (Str "custom")] [Text "Anything"]
      anchored = addHeadingIds h
   in case anchored of
        Element "h2" attrs _ => idOf attrs === Just "custom"
        _ => failWith Nothing "shape changed unexpectedly"

||| Non-heading elements are unchanged.
export
ext_addIds_skips_non_heading : Property
ext_addIds_skips_non_heading = oneShot $
  let h        = Element "p" [] [Text "no id here"]
      anchored = addHeadingIds h
   in case anchored of
        Element "p" attrs _ => idOf attrs === Nothing
        _ => failWith Nothing "shape changed unexpectedly"

||| Two sibling headings with identical text get disambiguated:
||| first gets the base slug; second gets `-2`.
export
ext_addIds_disambiguates_duplicates : Property
ext_addIds_disambiguates_duplicates = oneShot $
  let tree     = Element "section" []
                   [ hExpr_h2 "Phase"
                   , hExpr_h2 "Phase"
                   ]
      anchored = addHeadingIds tree
   in case anchored of
        Element "section" _ [Element "h2" a1 _, Element "h2" a2 _] => do
          idOf a1 === Just "phase"
          idOf a2 === Just "phase-2"
        _ => failWith Nothing "shape changed unexpectedly"

||| `addHeadingIds . addHeadingIds = addHeadingIds`.
export
ext_addIds_idempotent : Property
ext_addIds_idempotent = oneShot $
  let tree    = Element "section" []
                  [ hExpr_h2 "Alpha"
                  , Element "h3" [] [Text "Beta"]
                  ]
      once    = addHeadingIds tree
      twice   = addHeadingIds once
   in twice === once

--------------------------------------------------------------------------------
-- harvestHeadings EXTs.
--------------------------------------------------------------------------------

||| Document order: pre-order walk; anchors from the existing `id`.
export
ext_harvest_in_document_order : Property
ext_harvest_in_document_order = oneShot $
  let tree    = Element "main" []
                  [ Element "h1" [MkHAttr "id" (Str "title")]   [Text "Title"]
                  , Element "h2" [MkHAttr "id" (Str "first")]   [Text "First"]
                  , Element "h3" [MkHAttr "id" (Str "first-a")] [Text "First.A"]
                  , Element "h2" [MkHAttr "id" (Str "second")]  [Text "Second"]
                  ]
      rows    = harvestHeadings tree
   in rows === [ (1, "title",   "Title")
               , (2, "first",   "First")
               , (3, "first-a", "First.A")
               , (2, "second",  "Second")
               ]

||| harvestHeadings on a tree with no headings is empty.
export
ext_harvest_no_headings : Property
ext_harvest_no_headings = oneShot $
  harvestHeadings (Element "p" [] [Text "no headings"]) === []

||| addHeadingIds + harvestHeadings round-trip: every harvested row has a
||| non-empty anchor that matches `slugify (plainText)` (modulo disambig).
export
ext_harvest_after_anchor_roundtrip : Property
ext_harvest_after_anchor_roundtrip = oneShot $
  let tree     = Element "main" []
                   [ Element "h2" [] [Text "Foo Bar"]
                   , Element "h3" [] [Text "Baz"]
                   ]
      anchored = addHeadingIds tree
      rows     = harvestHeadings anchored
   in rows === [ (2, "foo-bar", "Foo Bar")
               , (3, "baz",     "Baz")
               ]

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "Cribrum.Pipeline.Anchor"
  [ ("ext_slugify_simple",                  ext_slugify_simple)
  , ("ext_slugify_lowercases",              ext_slugify_lowercases)
  , ("ext_slugify_collapses_runs",          ext_slugify_collapses_runs)
  , ("ext_slugify_trims_edges",             ext_slugify_trims_edges)
  , ("ext_slugify_empty",                   ext_slugify_empty)
  , ("ext_slugify_only_punct",              ext_slugify_only_punct)
  , ("ext_slugify_idempotent",              ext_slugify_idempotent)
  , ("pddt_slugify_table",                  pddt_slugify_table)
  , ("ext_addIds_assigns_slug_from_text",   ext_addIds_assigns_slug_from_text)
  , ("ext_addIds_preserves_existing",       ext_addIds_preserves_existing)
  , ("ext_addIds_skips_non_heading",        ext_addIds_skips_non_heading)
  , ("ext_addIds_disambiguates_duplicates", ext_addIds_disambiguates_duplicates)
  , ("ext_addIds_idempotent",               ext_addIds_idempotent)
  , ("ext_harvest_in_document_order",       ext_harvest_in_document_order)
  , ("ext_harvest_no_headings",             ext_harvest_no_headings)
  , ("ext_harvest_after_anchor_roundtrip",  ext_harvest_after_anchor_roundtrip)
  ]
