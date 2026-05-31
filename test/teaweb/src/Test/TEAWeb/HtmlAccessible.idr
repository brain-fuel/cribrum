||| EXT + PDDT for `TEAWeb.Html.Accessible` — the `StructuralAA` view
||| codomain.
|||
||| Properties exercised:
|||   - `decAccessibleView` ACCEPTS a valid, accessible view and returns an
|||     `AccessibleView` whose `tree` is the view's tree and whose `handlers`
|||     are preserved (EXT). The very existence of the returned value is the
|||     proof: its fields demand `IsValidHtml tree` and `StructuralAA tree`,
|||     discharged from the decision procedures.
|||   - `decAccessibleView` REJECTS a view that fails a Phase-4 structural
|||     rule (img without alt → `NotAccessible "img-alt"`), and one that
|||     fails the Phase-2 content model (block in phrasing →
|||     `NotValidHtml`). These are the runtime-observable half of the
|||     boundary (EXT). Their *static* counterpart — that you cannot build
|||     the field of an `AccessibleProgram.view` from a rejected view — is
|||     unrepresentable as a runtime value, exactly as with the typed-child
|||     negatives in `Test.TEAWeb.HtmlTyped`.
|||   - `toView` round-trips: projecting an accepted `AccessibleView` back to
|||     a `View msg` recovers the original tree + handlers (EXT).
|||   - PDDT: a sweep of accessible views all pass; a sweep of inaccessible
|||     views all fail with the expected rule id.
module Test.TEAWeb.HtmlAccessible

import Data.List
import Hedgehog
import Cribrum.Node
import TEAWeb.Html
import TEAWeb.Html.Accessible
import TEAWeb.Event

%default total

--------------------------------------------------------------------------------
-- Test-only msg.
--------------------------------------------------------------------------------

data Msg = MNoop | MClick

Eq Msg where
  MNoop  == MNoop  = True
  MClick == MClick = True
  _      == _      = False

Show Msg where
  show MNoop  = "MNoop"
  show MClick = "MClick"

--------------------------------------------------------------------------------
-- Representative accessible / inaccessible views.
--------------------------------------------------------------------------------

||| A valid, accessible view: a section whose anchors carry href + text,
||| whose images carry alt, with no heading skip. Passes both gates.
accessibleView : View Msg
accessibleView =
  section_ []
    [ h1_ [] [text_ "Title"]
    , p_  []
        [ text_ "see "
        , a_ [href_ "/docs"] [text_ "the docs"]
        ]
    , img_ [src_ "/logo.png", alt_ "Cribrum logo"] []
    ]

||| An accessible view with an event handler, to confirm the handler table
||| survives the boundary.
accessibleWithHandler : View Msg
accessibleWithHandler =
  div_ []
    [ button_ [onClick "go" MClick] [text_ "Go"] ]

||| Fails Phase 4 (img-alt): an image with no alt attribute. Valid HTML,
||| inaccessible.
imgNoAltView : View Msg
imgNoAltView =
  div_ [] [ img_ [src_ "/logo.png"] [] ]

||| Fails Phase 4 (anchor-href): an anchor with no href.
anchorNoHrefView : View Msg
anchorNoHrefView =
  p_ [] [ a_ [] [text_ "nowhere"] ]

||| Fails Phase 2 (block-in-phrasing): a div inside a p. Rejected before the
||| accessibility gate even runs.
blockInPhrasingView : View Msg
blockInPhrasingView =
  p_ [] [ div_ [] [text_ "illegal"] ]

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

||| An accessible view is accepted; the returned `AccessibleView` (whose
||| fields demand `IsValidHtml` + `StructuralAA` witnesses) carries the same
||| tree.
export
ext_accessible_view_accepted : Property
ext_accessible_view_accepted = withTests 1 . property $
  case decAccessibleView accessibleView of
    Left err => failWith Nothing ("expected acceptance, got: " ++ show err)
    Right av => tree av === tree accessibleView

||| The handler table survives the boundary unchanged.
export
ext_accessible_preserves_handlers : Property
ext_accessible_preserves_handlers = withTests 1 . property $
  case decAccessibleView accessibleWithHandler of
    Left err => failWith Nothing ("expected acceptance, got: " ++ show err)
    Right av => do
      length (handlers av) === 1
      map fst (handlers av) === ["go"]

||| `toView` recovers the original tree + handlers from an accepted view.
export
ext_toView_roundtrips : Property
ext_toView_roundtrips = withTests 1 . property $
  case decAccessibleView accessibleWithHandler of
    Left err => failWith Nothing ("expected acceptance, got: " ++ show err)
    Right av => do
      tree (toView av) === tree accessibleWithHandler
      map fst (handlers (toView av)) === map fst (handlers accessibleWithHandler)

||| An img without alt is rejected as inaccessible, naming the img-alt rule.
export
ext_img_no_alt_rejected : Property
ext_img_no_alt_rejected = withTests 1 . property $
  case decAccessibleView imgNoAltView of
    Right _                  => failWith Nothing "expected rejection (img-alt)"
    Left (NotAccessible r _) => r === "img-alt"
    Left (NotValidHtml ve)   =>
      failWith Nothing ("expected NotAccessible img-alt, got NotValidHtml: " ++ show ve)

||| An anchor without href is rejected as inaccessible, naming anchor-href.
export
ext_anchor_no_href_rejected : Property
ext_anchor_no_href_rejected = withTests 1 . property $
  case decAccessibleView anchorNoHrefView of
    Right _                  => failWith Nothing "expected rejection (anchor-href)"
    Left (NotAccessible r _) => r === "anchor-href"
    Left (NotValidHtml ve)   =>
      failWith Nothing ("expected NotAccessible anchor-href, got NotValidHtml: " ++ show ve)

||| A block element inside a phrasing element is rejected by the Phase-2
||| gate before the accessibility check runs.
export
ext_block_in_phrasing_rejected : Property
ext_block_in_phrasing_rejected = withTests 1 . property $
  case decAccessibleView blockInPhrasingView of
    Right _                => failWith Nothing "expected NotValidHtml rejection"
    Left (NotValidHtml _)  => success
    Left (NotAccessible r _) =>
      failWith Nothing ("expected NotValidHtml, got NotAccessible: " ++ r)

--------------------------------------------------------------------------------
-- PDDT — sweeps.
--------------------------------------------------------------------------------

accessibleCorpus : List (View Msg)
accessibleCorpus =
  [ accessibleView
  , accessibleWithHandler
  , p_ [] [text_ "plain"]
  , div_ [] [ h1_ [] [text_ "h"], p_ [] [text_ "body"] ]
  , a_ [href_ "/x"] [text_ "named link"]
  ]

export
pddt_accessible_corpus_all_accepted : Property
pddt_accessible_corpus_all_accepted = withTests 1 . property $
  for_ accessibleCorpus $ \v =>
    case decAccessibleView v of
      Right _  => success
      Left err => failWith Nothing ("expected acceptance, got: " ++ show err)

||| (inaccessible view, expected rule id).
inaccessibleCorpus : List (View Msg, String)
inaccessibleCorpus =
  [ (imgNoAltView,     "img-alt")
  , (anchorNoHrefView, "anchor-href")
  ]

export
pddt_inaccessible_corpus_named_rule : Property
pddt_inaccessible_corpus_named_rule = withTests 1 . property $
  for_ inaccessibleCorpus $ \(v, expectedRule) =>
    case decAccessibleView v of
      Right _                  => failWith Nothing ("expected rejection: " ++ expectedRule)
      Left (NotAccessible r _) => r === expectedRule
      Left (NotValidHtml ve)   =>
        failWith Nothing ("expected NotAccessible " ++ expectedRule
                            ++ ", got NotValidHtml: " ++ show ve)

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "TEAWeb.Html.Accessible"
  [ ("ext_accessible_view_accepted",        ext_accessible_view_accepted)
  , ("ext_accessible_preserves_handlers",   ext_accessible_preserves_handlers)
  , ("ext_toView_roundtrips",               ext_toView_roundtrips)
  , ("ext_img_no_alt_rejected",             ext_img_no_alt_rejected)
  , ("ext_anchor_no_href_rejected",         ext_anchor_no_href_rejected)
  , ("ext_block_in_phrasing_rejected",      ext_block_in_phrasing_rejected)
  , ("pddt_accessible_corpus_all_accepted", pddt_accessible_corpus_all_accepted)
  , ("pddt_inaccessible_corpus_named_rule", pddt_inaccessible_corpus_named_rule)
  ]
