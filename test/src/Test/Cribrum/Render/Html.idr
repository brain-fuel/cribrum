module Test.Cribrum.Render.Html

import Data.String
import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.Render.Html
import Test.Cribrum.Gen

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_text_escaped : Property
ext_text_escaped = oneShot $
  renderHtml (Text "a < b & c") === "a &lt; b &amp; c"

export
ext_text_quotes_escaped : Property
ext_text_quotes_escaped = oneShot $
  renderHtml (Text "she said \"hi\" & it's nice")
    === "she said &quot;hi&quot; &amp; it&#39;s nice"

export
ext_comment_unescaped : Property
ext_comment_unescaped = oneShot $
  renderHtml (Comment "x < y") === "<!-- x < y -->"

export
ext_empty_p : Property
ext_empty_p = oneShot $
  renderHtml (Element "p" [] []) === "<p></p>"

export
ext_p_with_text : Property
ext_p_with_text = oneShot $
  renderHtml (Element "p" [] [Text "hi"]) === "<p>hi</p>"

export
ext_nested_section : Property
ext_nested_section = oneShot $
  renderHtml
    (Element "section" []
      [ Element "h1" [] [Text "T"]
      , Element "p"  [] [Text "B"]
      ])
    === "<section><h1>T</h1><p>B</p></section>"

export
ext_void_br_no_close : Property
ext_void_br_no_close = oneShot $
  renderHtml (Element "br" [] []) === "<br>"

export
ext_void_hr_no_close : Property
ext_void_hr_no_close = oneShot $
  renderHtml (Element "hr" [] []) === "<hr>"

export
ext_void_img_with_attrs : Property
ext_void_img_with_attrs = oneShot $
  renderHtml
    (Element "img"
       [ MkHAttr "src" (Str "/x.png")
       , MkHAttr "alt" (Str "x")
       ]
       [])
    === "<img src=\"/x.png\" alt=\"x\">"

export
ext_attr_value_escaped : Property
ext_attr_value_escaped = oneShot $
  renderHtml
    (Element "a"
       [ MkHAttr "href" (Str "https://example.com?a=1&b=\"x\"") ]
       [Text "go"])
    === "<a href=\"https://example.com?a=1&amp;b=&quot;x&quot;\">go</a>"

export
ext_handler_renders_as_data_attribute : Property
ext_handler_renders_as_data_attribute = oneShot $
  renderHtml
    (Element "button"
       [ MkHAttr "onclick" (Handler "click" "cb1") ]
       [Text "go"])
    === "<button data-on-click=\"cb1\">go</button>"

export
ext_multiple_attrs : Property
ext_multiple_attrs = oneShot $
  renderHtml
    (Element "div"
       [ MkHAttr "id"    (Str "main")
       , MkHAttr "class" (Str "panel")
       ]
       [])
    === "<div id=\"main\" class=\"panel\"></div>"

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

||| Every void element renders without a closing tag.
voidRenderCases : List (String, String)
voidRenderCases =
  [ ("area",   "<area>")
  , ("base",   "<base>")
  , ("br",     "<br>")
  , ("col",    "<col>")
  , ("embed",  "<embed>")
  , ("hr",     "<hr>")
  , ("img",    "<img>")
  , ("input",  "<input>")
  , ("link",   "<link>")
  , ("meta",   "<meta>")
  , ("source", "<source>")
  , ("track",  "<track>")
  , ("wbr",    "<wbr>")
  ]

export
pddt_void_elements : Property
pddt_void_elements = withTests 1 . property $ do
  for_ voidRenderCases $ \(tag, expected) =>
    renderHtml (Element tag [] []) === expected

||| Text-escaping table.
escapeCases : List (String, String)
escapeCases =
  [ ("",         "")
  , ("plain",    "plain")
  , ("<",        "&lt;")
  , (">",        "&gt;")
  , ("&",        "&amp;")
  , ("\"",       "&quot;")
  , ("'",        "&#39;")
  , ("a<b>c&d",  "a&lt;b&gt;c&amp;d")
  ]

export
pddt_text_escape : Property
pddt_text_escape = withTests 1 . property $ do
  for_ escapeCases $ \(input, expected) =>
    renderHtml (Text input) === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| Render is total over arbitrary HExpr.
export
pbt_render_total : Property
pbt_render_total = property $ do
  h <- forAll hexpr
  let s = renderHtml h
  -- Trivially observe; if `renderHtml` were partial we'd diverge.
  diff (length s) (>=) 0

||| The rendered output of a non-void element always contains both its
||| opening and closing tags.
export
pbt_non_void_has_close : Property
pbt_non_void_has_close = property $ do
  -- Restrict tag to a known non-void element to keep the property focused.
  let nonVoid = element $ the (Vect _ String)
        [ "p", "div", "span", "section", "article"
        , "h1", "ul", "li", "a", "main", "nav"
        ]
  tag <- forAll nonVoid
  cs  <- forAll (list (linear 0 3) hexpr)
  let r = renderHtml (Element tag [] cs)
  diff ("<" ++ tag ++ ">")  isInfixOf r
  diff ("</" ++ tag ++ ">") isInfixOf r

||| Void elements never carry a closing tag in the output.
export
pbt_void_has_no_close : Property
pbt_void_has_no_close = property $ do
  let voidGen = element $ the (Vect _ String)
        ["br", "hr", "img", "meta", "link", "input"]
  tag <- forAll voidGen
  let r = renderHtml (Element tag [] [])
  let closing = "</" ++ tag ++ ">"
  classify "got close tag (BAD)" (closing `isInfixOf` r)
  assert (not (closing `isInfixOf` r))

export
group : Group
group = MkGroup "Cribrum.Render.Html"
  [ ("ext_text_escaped",                  ext_text_escaped)
  , ("ext_text_quotes_escaped",           ext_text_quotes_escaped)
  , ("ext_comment_unescaped",             ext_comment_unescaped)
  , ("ext_empty_p",                       ext_empty_p)
  , ("ext_p_with_text",                   ext_p_with_text)
  , ("ext_nested_section",                ext_nested_section)
  , ("ext_void_br_no_close",              ext_void_br_no_close)
  , ("ext_void_hr_no_close",              ext_void_hr_no_close)
  , ("ext_void_img_with_attrs",           ext_void_img_with_attrs)
  , ("ext_attr_value_escaped",            ext_attr_value_escaped)
  , ("ext_handler_renders_as_data_attribute",
        ext_handler_renders_as_data_attribute)
  , ("ext_multiple_attrs",                ext_multiple_attrs)
  , ("pddt_void_elements",                pddt_void_elements)
  , ("pddt_text_escape",                  pddt_text_escape)
  , ("pbt_render_total",                  pbt_render_total)
  , ("pbt_non_void_has_close",            pbt_non_void_has_close)
  , ("pbt_void_has_no_close",             pbt_void_has_no_close)
  ]
