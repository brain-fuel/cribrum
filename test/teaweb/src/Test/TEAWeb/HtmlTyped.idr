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
  , ("ext_unTyped_projects_view",                ext_unTyped_projects_view)
  , ("pddt_typed_predicate_matches_catalog",     pddt_typed_predicate_matches_catalog)
  , ("pddt_typed_shorthand_tags",                pddt_typed_shorthand_tags)
  , ("pbt_typed_button_handlers_propagate",      pbt_typed_button_handlers_propagate)
  ]
