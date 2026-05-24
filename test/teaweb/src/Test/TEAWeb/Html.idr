||| EXT + PDDT + PBT for `TEAWeb.Html`.
|||
||| Properties exercised:
|||   - Smart constructors build HExpr with the expected tag.
|||   - `text_` becomes a `Text` node; `comment_` becomes a `Comment`.
|||   - `splitAttrs` is via `mkElement`: handler attrs propagate into the
|||     view's HandlerTable.
|||   - Children's handler tables are concatenated into the parent's.
|||   - `viewSafe` accepts well-formed views and rejects unknown-tag views.
module Test.TEAWeb.Html

import Data.List
import Hedgehog
import Cribrum.Node
import Cribrum.Html.Valid
import TEAWeb.Html
import TEAWeb.Event

%default total

--------------------------------------------------------------------------------
-- Test-only msg.
--------------------------------------------------------------------------------

data Msg = MIncrement | MDecrement | MFocus

Eq Msg where
  MIncrement == MIncrement = True
  MDecrement == MDecrement = True
  MFocus     == MFocus     = True
  _          == _          = False

Show Msg where
  show MIncrement = "MIncrement"
  show MDecrement = "MDecrement"
  show MFocus     = "MFocus"

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_text_becomes_text_node : Property
ext_text_becomes_text_node = withTests 1 . property $ do
  let v : View Msg
      v = text_ "hello"
  tree v === Text "hello"
  length (handlers v) === 0

export
ext_comment_becomes_comment_node : Property
ext_comment_becomes_comment_node = withTests 1 . property $ do
  let v : View Msg
      v = comment_ "note"
  tree v === Comment "note"

export
ext_div_with_class_emits_element : Property
ext_div_with_class_emits_element = withTests 1 . property $ do
  let v : View Msg
      v = div_ [class_ "main"] [text_ "hi"]
  case tree v of
    Element t a cs => do
      t === "div"
      a === [MkHAttr "class" (Str "main")]
      cs === [Text "hi"]
    _ => failWith Nothing "expected Element"

export
ext_button_with_onclick_registers_handler : Property
ext_button_with_onclick_registers_handler = withTests 1 . property $ do
  let v : View Msg
      v = button_ [onClick "inc-btn" MIncrement] [text_ "+"]
  -- The button has a data-on-click attribute carrying the callback id.
  case tree v of
    Element t a cs => do
      t === "button"
      a === [MkHAttr "data-on-click" (Handler "click" "inc-btn")]
      cs === [Text "+"]
    _ => failWith Nothing "expected Element"
  -- The handler is registered against its callback id.
  length (handlers v) === 1
  case handlers v of
    [(cb, fn)] => do
      cb === "inc-btn"
      -- The closure produces the registered msg regardless of event.
      let dummyEvent = believe_me {b = Event} ()
      unsafePerformIO (fn dummyEvent) === MIncrement
    _ => failWith Nothing "expected exactly one handler"

export
ext_nested_views_concat_handlers : Property
ext_nested_views_concat_handlers = withTests 1 . property $ do
  let v : View Msg
      v = div_ []
            [ button_ [onClick "inc" MIncrement] [text_ "+"]
            , button_ [onClick "dec" MDecrement] [text_ "-"]
            ]
  length (handlers v) === 2
  map fst (handlers v) === ["inc", "dec"]

export
ext_void_element_has_no_children : Property
ext_void_element_has_no_children = withTests 1 . property $ do
  let v : View Msg
      v = hr_ [class_ "sep"]
  case tree v of
    Element t a cs => do
      t === "hr"
      a === [MkHAttr "class" (Str "sep")]
      cs === []
    _ => failWith Nothing "expected Element"

export
ext_viewSafe_accepts_known_tags : Property
ext_viewSafe_accepts_known_tags = withTests 1 . property $ do
  let v : View Msg
      v = main_ [] [ h1_ [] [text_ "title"], p_ [] [text_ "body"] ]
  case viewSafe v of
    Right _ => success
    Left  e => failWith Nothing ("expected accepted view, got " ++ show e)

export
ext_viewSafe_rejects_unknown_tag : Property
ext_viewSafe_rejects_unknown_tag = withTests 1 . property $ do
  -- "marquee" is not in the spike's knownTags set.
  let v : View Msg
      v = MkView (Element "marquee" [] [Text "scroll"]) []
  case viewSafe v of
    Right _ => failWith Nothing "expected rejection"
    Left  _ => success

--------------------------------------------------------------------------------
-- PDDT — every shorthand maps to the correct tag.
--------------------------------------------------------------------------------

shorthandCases : List (View Msg, String)
shorthandCases =
  [ (div_      [] [], "div")
  , (span_     [] [], "span")
  , (p_        [] [], "p")
  , (h1_       [] [], "h1")
  , (h2_       [] [], "h2")
  , (h3_       [] [], "h3")
  , (h4_       [] [], "h4")
  , (h5_       [] [], "h5")
  , (h6_       [] [], "h6")
  , (a_        [] [], "a")
  , (button_   [] [], "button")
  , (input_    [] [], "input")
  , (form_     [] [], "form")
  , (ul_       [] [], "ul")
  , (ol_       [] [], "ol")
  , (li_       [] [], "li")
  , (section_  [] [], "section")
  , (article_  [] [], "article")
  , (aside_    [] [], "aside")
  , (main_     [] [], "main")
  , (nav_      [] [], "nav")
  , (header_   [] [], "header")
  , (footer_   [] [], "footer")
  , (figure_   [] [], "figure")
  , (figcaption_ [] [], "figcaption")
  , (img_      [] [], "img")
  , (label_    [] [], "label")
  , (strong_   [] [], "strong")
  , (em_       [] [], "em")
  , (code_     [] [], "code")
  , (pre_      [] [], "pre")
  , (blockquote_ [] [], "blockquote")
  , (br_       [],    "br")
  , (hr_       [],    "hr")
  ]

export
pddt_shorthand_tags : Property
pddt_shorthand_tags = withTests 1 . property $
  for_ shorthandCases $ \(v, expectedTag) =>
    case tree v of
      Element t _ _ => t === expectedTag
      _             => failWith Nothing ("expected element with tag " ++ expectedTag)

--------------------------------------------------------------------------------
-- PBT — handlers from N onClick attrs always appear in the handler table.
--------------------------------------------------------------------------------

export
pbt_onclick_handlers_propagate : Property
pbt_onclick_handlers_propagate = property $ do
  ids <- forAll $ Gen.list (Range.linear 0 10) (Gen.string (Range.linear 1 4) Gen.alphaNum)
  let attrs : List (Attr Msg)
      attrs = map (\i => onClick i MIncrement) ids
      v     = button_ attrs [text_ "x"]
  length (handlers v) === length ids
  map fst (handlers v) === ids

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "TEAWeb.Html"
  [ ("ext_text_becomes_text_node",            ext_text_becomes_text_node)
  , ("ext_comment_becomes_comment_node",      ext_comment_becomes_comment_node)
  , ("ext_div_with_class_emits_element",      ext_div_with_class_emits_element)
  , ("ext_button_with_onclick_registers_handler", ext_button_with_onclick_registers_handler)
  , ("ext_nested_views_concat_handlers",      ext_nested_views_concat_handlers)
  , ("ext_void_element_has_no_children",      ext_void_element_has_no_children)
  , ("ext_viewSafe_accepts_known_tags",       ext_viewSafe_accepts_known_tags)
  , ("ext_viewSafe_rejects_unknown_tag",      ext_viewSafe_rejects_unknown_tag)
  , ("pddt_shorthand_tags",                   pddt_shorthand_tags)
  , ("pbt_onclick_handlers_propagate",        pbt_onclick_handlers_propagate)
  ]
