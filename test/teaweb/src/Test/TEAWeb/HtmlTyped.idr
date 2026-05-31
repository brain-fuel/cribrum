||| EXT + PDDT + PBT for `TEAWeb.Html.Typed`.
|||
||| Properties exercised:
|||   - Each typed smart constructor emits the right HExpr tag and
|||     `TypedView <tag> msg` carrier (PDDT).
|||   - Children wrapped via `c_` are statically checked against the
|||     catalog: every positive case here compiles, demonstrating that
|||     auto-discharge of `So (isTagAllowedIn parent child)` succeeds for
|||     the entries in the policy (EXT + nested view EXT).
|||   - The handler table is preserved through `typedElement_`'s
|||     `mkElement` core; nested handlers concat upwards (EXT).
|||   - `isTagAllowedIn` agrees with the catalog (`childAllowedBool`
|||     specialised to `Element child [] []`) — drift gate against the
|||     single source of truth (PDDT).
|||
||| Negative tests (placement rejected at compile time) are
||| unrepresentable as runtime values — by design, the offending program
||| would fail to typecheck. They appear in `docs/conventions.md` as
||| worked examples rather than runtime assertions.
module Test.TEAWeb.HtmlTyped

import Data.List
import Hedgehog
import Cribrum.Node
import Cribrum.Html.Valid
import TEAWeb.Html
import TEAWeb.Html.Typed
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
-- EXTs.
--------------------------------------------------------------------------------

||| Typed `ul` over typed `li` children compiles and produces the expected
||| HExpr tree shape.
export
ext_ulT_with_liT_children_compiles : Property
ext_ulT_with_liT_children_compiles = withTests 1 . property $ do
  let v : TypedView "ul" Msg
      v = ulT_ []
            [ c_ (liT_ [] [tx_ "a"])
            , c_ (liT_ [] [tx_ "b"])
            ]
  case tree (view v) of
    Element t _ cs => do
      t === "ul"
      length cs === 2
    _              => failWith Nothing "expected Element"

||| Typed `section` accepts flow-content children (h1, p).
export
ext_sectionT_accepts_flow_children : Property
ext_sectionT_accepts_flow_children = withTests 1 . property $ do
  let v : TypedView "section" Msg
      v = sectionT_ []
            [ c_ (h1T_ [] [tx_ "title"])
            , c_ (pT_  [] [tx_ "body"])
            ]
  case tree (view v) of
    Element t _ cs => do
      t === "section"
      length cs === 2
    _              => failWith Nothing "expected Element"

||| `buttonT_` accepts phrasing children (text); the handler from
||| `onClick` survives the typed wrapper.
export
ext_buttonT_carries_handler : Property
ext_buttonT_carries_handler = withTests 1 . property $ do
  let v : TypedView "button" Msg
      v = buttonT_ [onClick "go" MClick] [tx_ "go"]
  case tree (view v) of
    Element t a cs => do
      t === "button"
      a === [MkHAttr "data-on-click" (Handler "click" "go")]
      cs === [Text "go"]
    _              => failWith Nothing "expected Element"
  length (handlers (view v)) === 1
  map fst (handlers (view v)) === ["go"]

||| Void-element typed wrappers produce childless HExpr.
export
ext_voidT_wrappers_are_childless : Property
ext_voidT_wrappers_are_childless = withTests 1 . property $ do
  case tree (view (brT_  {msg = Msg} [])) of
    Element t _ cs => do t === "br"  ; cs === []
    _              => failWith Nothing "expected br Element"
  case tree (view (hrT_  {msg = Msg} [])) of
    Element t _ cs => do t === "hr"  ; cs === []
    _              => failWith Nothing "expected hr Element"
  case tree (view (imgT_ {msg = Msg} [alt_ "x"])) of
    Element t _ cs => do t === "img" ; cs === []
    _              => failWith Nothing "expected img Element"
  case tree (view (inputT_ {msg = Msg} [type_ "checkbox"])) of
    Element t _ cs => do t === "input" ; cs === []
    _              => failWith Nothing "expected input Element"

||| Nested typed constructors compose and propagate handlers up the tree
||| just like the untyped `mkElement` path.
export
ext_nested_typed_views_concat_handlers : Property
ext_nested_typed_views_concat_handlers = withTests 1 . property $ do
  let v : TypedView "main" Msg
      v = mainT_ []
            [ c_ (navT_ []
                [ c_ (ulT_ []
                    [ c_ (liT_ [] [ c_ (aT_ [href_ "#a"] [tx_ "A"]) ])
                    , c_ (liT_ [] [ c_ (aT_ [href_ "#b"] [tx_ "B"]) ])
                    ])
                ])
            , c_ (sectionT_ []
                [ c_ (h1T_ [] [tx_ "title"])
                , c_ (pT_  [] [ c_ (buttonT_ [onClick "c1" MClick] [tx_ "c1"]) ])
                , c_ (pT_  [] [ c_ (buttonT_ [onClick "c2" MClick] [tx_ "c2"]) ])
                ])
            ]
  length (handlers (view v)) === 2
  map fst (handlers (view v)) === ["c1", "c2"]

||| Well-formed table builds from typed constructors: caption, colgroup
||| of cols, thead/tbody of trs, trs of th/td cells. Every child position
||| is auto-discharged against the catalog content model — this whole
||| expression typechecking *is* the positive content-model proof.
export
ext_tableT_well_formed_builds : Property
ext_tableT_well_formed_builds = withTests 1 . property $ do
  let v : TypedView "table" Msg
      v = tableT_ []
            [ c_ (captionT_  [] [tx_ "Scores"])
            , c_ (colgroupT_ [] [ c_ (colT_ []), c_ (colT_ []) ])
            , c_ (theadT_ []
                [ c_ (trT_ []
                    [ c_ (thT_ [] [tx_ "Name"])
                    , c_ (thT_ [] [tx_ "Pts"])
                    ])
                ])
            , c_ (tbodyT_ []
                [ c_ (trT_ []
                    [ c_ (tdT_ [] [tx_ "Ada"])
                    , c_ (tdT_ [] [ c_ (strongT_ [] [tx_ "99"]) ])
                    ])
                ])
            ]
  case tree (view v) of
    Element t _ cs => do
      t === "table"
      length cs === 4
    _              => failWith Nothing "expected table Element"
  -- The whole typed tree is, by construction, valid HTML: route the
  -- underlying view through the Phase-2 gate as a belt-and-braces check.
  case viewSafe (unTyped v) of
    Right _ => pure ()
    Left e  => failWith Nothing ("typed table should pass viewSafe: " ++ show e)

||| Negative side of the guarantee, at the *catalog* level: a stray `<p>`
||| placed directly under `<table>` (which the typed `tableT_` constructor
||| makes a compile error, since `isTagAllowedIn "table" "p" = False` does
||| not auto-discharge) is rejected by the same content model the typed
||| layer consumes. We build the malformed tree with the *untyped*
||| constructors — the only way to express it as a runtime value — and
||| show the model rejects it with the table-structure rejection class.
export
ext_malformed_table_rejected_by_model : Property
ext_malformed_table_rejected_by_model = withTests 1 . property $ do
  -- `isTagAllowedIn "table" "p"` is False, so `c_ (pT_ ...)` under a
  -- `tableT_` would fail to typecheck. The validator agrees:
  isTagAllowedIn "table" "p" === False
  let bad : View Msg
      bad = mkElement "table" [] [ mkElement "p" [] [text_ "loose"] ]
  case viewSafe bad of
    Right _                            =>
      failWith Nothing "expected viewSafe to reject p directly under table"
    Left (InvalidHtml (MkLocatedReject path reason)) => do
      path === [0]
      case reason of
        MalformedTable parent child => do
          parent === "table"
          child  === "p"
        other => failWith Nothing ("expected MalformedTable, got " ++ show other)

||| A bare `<td>` directly under `<table>` (skipping the required
||| row/section nesting) is likewise rejected by the catalog the typed
||| layer reuses; `isTagAllowedIn` refuses to discharge it.
export
ext_td_directly_in_table_rejected : Property
ext_td_directly_in_table_rejected = withTests 1 . property $ do
  isTagAllowedIn "table" "td" === False
  isTagAllowedIn "tr"    "td" === True
  let bad : View Msg
      bad = mkElement "table" [] [ mkElement "td" [] [text_ "x"] ]
  case viewSafe bad of
    Right _ => failWith Nothing "expected viewSafe to reject td directly under table"
    Left _  => pure ()

||| `unTyped` exposes the underlying `View msg` to the rest of the
||| TEAWeb pipeline (Program.view, viewSafe, etc.).
export
ext_unTyped_projects_view : Property
ext_unTyped_projects_view = withTests 1 . property $ do
  let v : TypedView "p" Msg
      v = pT_ [] [tx_ "hi"]
      u : View Msg
      u = unTyped v
  case tree u of
    Element t _ cs => do
      t === "p"
      cs === [Text "hi"]
    _              => failWith Nothing "expected p Element"

--------------------------------------------------------------------------------
-- Drift gate: `isTagAllowedIn` agrees with `childAllowedBool` over a
-- catalog-anchored set of (parent, child-tag) pairs. The two functions
-- are independent implementations of the same content-model rule;
-- equality here pins the typed surface to the validator.
--------------------------------------------------------------------------------

||| Parents typed in this module + a sweep of candidate child tags. The
||| drift gate asserts `isTagAllowedIn parent child` matches the catalog
||| (`childAllowedBool parent (Element child [] [])`) on every pair.
||| The catalog is the authoritative source of truth; the hand-coded
||| typed surface lives in parity with it via this test.
typedParents : List String
typedParents =
  [ "div", "span", "p", "h1", "h2", "h3", "h4", "h5", "h6"
  , "a", "button", "form", "ul", "ol", "li", "section", "article"
  , "aside", "main", "nav", "header", "footer", "figure", "figcaption"
  , "label", "strong", "em", "code", "pre", "blockquote"
  , "br", "hr", "img", "input"
  ]

childCandidates : List String
childCandidates =
  [ "div", "span", "p", "h1", "h2", "a", "button", "ul", "ol", "li"
  , "section", "img", "input", "br", "em", "strong", "label", "form"
  , "figure", "figcaption", "nav", "header", "footer", "script", "template"
  ]

driftPairs : List (String, String)
driftPairs =
  [ (p, c) | p <- typedParents, c <- childCandidates ]

export
pddt_typed_predicate_matches_catalog : Property
pddt_typed_predicate_matches_catalog = withTests 1 . property $
  for_ driftPairs $ \(parent, childTag) =>
    isTagAllowedIn parent childTag
      === childAllowedBool parent (Element childTag [] [])

||| Table-family drift gate. Same single-source-of-truth pin as above, but
||| over the table parents + table-relevant child tags. Every typed table
||| constructor's permitted-child rule must equal the catalog
||| (`childAllowedBool`) — so the compile-time `So (isTagAllowedIn ...)`
||| witness the typed table layer discharges is exactly the catalog's
||| content-model decision, never a hand-coded second copy.
tableParents : List String
tableParents =
  [ "table", "caption", "colgroup", "thead", "tbody", "tfoot", "tr"
  , "td", "th"
  ]

tableChildCandidates : List String
tableChildCandidates =
  [ "caption", "colgroup", "col", "thead", "tbody", "tfoot", "tr"
  , "td", "th", "p", "div", "span", "li", "ul", "script", "template"
  , "strong", "a"
  ]

tableDriftPairs : List (String, String)
tableDriftPairs =
  [ (p, c) | p <- tableParents, c <- tableChildCandidates ]

export
pddt_typed_table_predicate_matches_catalog : Property
pddt_typed_table_predicate_matches_catalog = withTests 1 . property $
  for_ tableDriftPairs $ \(parent, childTag) =>
    isTagAllowedIn parent childTag
      === childAllowedBool parent (Element childTag [] [])

--------------------------------------------------------------------------------
-- PDDT — every typed shorthand produces an Element with the expected tag.
--------------------------------------------------------------------------------

shorthandT : List (View Msg, String)
shorthandT =
  [ (view (divT_       [] []), "div")
  , (view (spanT_      [] []), "span")
  , (view (pT_         [] []), "p")
  , (view (h1T_        [] []), "h1")
  , (view (h2T_        [] []), "h2")
  , (view (h3T_        [] []), "h3")
  , (view (h4T_        [] []), "h4")
  , (view (h5T_        [] []), "h5")
  , (view (h6T_        [] []), "h6")
  , (view (aT_         [] []), "a")
  , (view (buttonT_    [] []), "button")
  , (view (formT_      [] []), "form")
  , (view (ulT_        [] []), "ul")
  , (view (olT_        [] []), "ol")
  , (view (liT_        [] []), "li")
  , (view (sectionT_   [] []), "section")
  , (view (articleT_   [] []), "article")
  , (view (asideT_     [] []), "aside")
  , (view (mainT_      [] []), "main")
  , (view (navT_       [] []), "nav")
  , (view (headerT_    [] []), "header")
  , (view (footerT_    [] []), "footer")
  , (view (figureT_    [] []), "figure")
  , (view (figcaptionT_ [] []), "figcaption")
  , (view (labelT_     [] []), "label")
  , (view (strongT_    [] []), "strong")
  , (view (emT_        [] []), "em")
  , (view (codeT_      [] []), "code")
  , (view (preT_       [] []), "pre")
  , (view (blockquoteT_ [] []), "blockquote")
  , (view (tableT_     [] []), "table")
  , (view (captionT_   [] []), "caption")
  , (view (colgroupT_  [] []), "colgroup")
  , (view (theadT_     [] []), "thead")
  , (view (tbodyT_     [] []), "tbody")
  , (view (tfootT_     [] []), "tfoot")
  , (view (trT_        [] []), "tr")
  , (view (tdT_        [] []), "td")
  , (view (thT_        [] []), "th")
  , (view (colT_       []),    "col")
  , (view (brT_        []),    "br")
  , (view (hrT_        []),    "hr")
  , (view (imgT_       []),    "img")
  , (view (inputT_     []),    "input")
  ]

export
pddt_typed_shorthand_tags : Property
pddt_typed_shorthand_tags = withTests 1 . property $
  for_ shorthandT $ \(v, expectedTag) =>
    case tree v of
      Element t _ _ => t === expectedTag
      _             => failWith Nothing ("expected element with tag " ++ expectedTag)

--------------------------------------------------------------------------------
-- PBT — handlers from N onClick attrs on a typed button survive.
--------------------------------------------------------------------------------

export
pbt_typed_button_handlers_propagate : Property
pbt_typed_button_handlers_propagate = property $ do
  ids <- forAll $ Gen.list (Range.linear 0 8)
                            (Gen.string (Range.linear 1 4) Gen.alphaNum)
  let attrs : List (Attr Msg)
      attrs = map (\i => onClick i MClick) ids
      v     = buttonT_ attrs [tx_ "x"]
  length (handlers (view v)) === length ids
  map fst (handlers (view v)) === ids

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "TEAWeb.Html.Typed"
  [ ("ext_ulT_with_liT_children_compiles",       ext_ulT_with_liT_children_compiles)
  , ("ext_sectionT_accepts_flow_children",       ext_sectionT_accepts_flow_children)
  , ("ext_buttonT_carries_handler",              ext_buttonT_carries_handler)
  , ("ext_voidT_wrappers_are_childless",         ext_voidT_wrappers_are_childless)
  , ("ext_nested_typed_views_concat_handlers",   ext_nested_typed_views_concat_handlers)
  , ("ext_tableT_well_formed_builds",            ext_tableT_well_formed_builds)
  , ("ext_malformed_table_rejected_by_model",    ext_malformed_table_rejected_by_model)
  , ("ext_td_directly_in_table_rejected",        ext_td_directly_in_table_rejected)
  , ("ext_unTyped_projects_view",                ext_unTyped_projects_view)
  , ("pddt_typed_predicate_matches_catalog",     pddt_typed_predicate_matches_catalog)
  , ("pddt_typed_table_predicate_matches_catalog", pddt_typed_table_predicate_matches_catalog)
  , ("pddt_typed_shorthand_tags",                pddt_typed_shorthand_tags)
  , ("pbt_typed_button_handlers_propagate",      pbt_typed_button_handlers_propagate)
  ]
