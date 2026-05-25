module Test.Cribrum.Html.Model

import Data.List
import Data.List.Quantifiers
import Data.Vect
import Hedgehog
import Cribrum.Html.Category
import Cribrum.Html.Model
import Cribrum.Html.Model.Invariants

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs — lookups.
--------------------------------------------------------------------------------

export
ext_lookup_div_present : Property
ext_lookup_div_present = oneShot $
  case lookupSpec "div" of
    Just s  => name s === "div"
    Nothing => failWith Nothing "div absent"

export
ext_lookup_unknown_absent : Property
ext_lookup_unknown_absent = oneShot $
  case lookupSpec "bogus" of
    Just _  => failWith Nothing "bogus should be absent"
    Nothing => success

export
ext_void_set_has_br_img_hr : Property
ext_void_set_has_br_img_hr = oneShot $ do
  isVoidTag "br"  === True
  isVoidTag "img" === True
  isVoidTag "hr"  === True
  isVoidTag "div" === False
  isVoidTag "p"   === False

export
ext_html_only_admits_head_body : Property
ext_html_only_admits_head_body = oneShot $
  case childPolicyOf "html" of
    OnlyTags tags False => tags === ["head", "body"]
    p                   =>
      failWith Nothing ("expected OnlyTags [head, body], got " ++ show p)

export
ext_p_is_phrasing_only : Property
ext_p_is_phrasing_only = oneShot $
  case childPolicyOf "p" of
    OnlyCategories cats => cats === [Phrasing]
    p                   =>
      failWith Nothing ("expected OnlyCategories [Phrasing], got " ++ show p)

export
ext_ul_admits_li_template_script : Property
ext_ul_admits_li_template_script = oneShot $
  case childPolicyOf "ul" of
    OnlyTags tags False => tags === ["li", "script", "template"]
    p                   =>
      failWith Nothing ("expected OnlyTags [li,script,template], got " ++ show p)

export
ext_categories_h1_includes_heading : Property
ext_categories_h1_includes_heading = oneShot $
  elem Heading (categoriesOf "h1") === True

export
ext_categories_span_includes_phrasing : Property
ext_categories_span_includes_phrasing = oneShot $
  elem Phrasing (categoriesOf "span") === True

export
ext_categories_unknown_empty : Property
ext_categories_unknown_empty = oneShot $
  categoriesOf "frobnicate" === []

export
ext_global_attr_class_recognized_on_any : Property
ext_global_attr_class_recognized_on_any = oneShot $ do
  isAllowedAttrName "div" "class"   === True
  isAllowedAttrName "span" "class"  === True
  isAllowedAttrName "main" "class"  === True

export
ext_data_attr_recognized : Property
ext_data_attr_recognized = oneShot $
  isAllowedAttrName "div" "data-anything" === True

export
ext_aria_attr_recognized : Property
ext_aria_attr_recognized = oneShot $
  isAllowedAttrName "button" "aria-pressed" === True

export
ext_on_event_attr_recognized : Property
ext_on_event_attr_recognized = oneShot $
  isAllowedAttrName "input" "onchange" === True

export
ext_href_attr_only_on_a_and_friends : Property
ext_href_attr_only_on_a_and_friends = oneShot $ do
  isAllowedAttrName "a"    "href" === True
  isAllowedAttrName "link" "href" === True
  isAllowedAttrName "area" "href" === True
  isAllowedAttrName "base" "href" === True
  isAllowedAttrName "div"  "href" === False
  isAllowedAttrName "span" "href" === False

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

voidTagCases : List String
voidTagCases =
  [ "area", "base", "br", "col", "embed", "hr"
  , "img", "input", "link", "meta", "source", "track", "wbr" ]

export
pddt_void_set_matches_html5 : Property
pddt_void_set_matches_html5 = oneShot $
  for_ voidTagCases $ \t =>
    isVoidTag t === True

attrAllowanceCases : List (String, String, Bool)
attrAllowanceCases =
  -- (tag, attrName, expected)
  [ ("a",        "href",          True)
  , ("a",        "target",        True)
  , ("img",      "src",           True)
  , ("img",      "alt",           True)
  , ("img",      "href",          False)
  , ("input",    "type",          True)
  , ("input",    "checked",       True)
  , ("button",   "type",          True)
  , ("button",   "disabled",      True)
  , ("form",     "action",        True)
  , ("p",        "style",         True)
  , ("p",        "checked",       False)
  ]

export
pddt_attr_allowance_table : Property
pddt_attr_allowance_table = oneShot $
  for_ attrAllowanceCases $ \(tag, attr, expected) =>
    isAllowedAttrName tag attr === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| `lookupSpec` is total: every tag returns either `Just spec` whose
||| `name` matches the query, or `Nothing` for tags not in the catalog.
export
pbt_lookup_round_trip : Property
pbt_lookup_round_trip = property $ do
  t <- forAll (element $ the (Vect _ String)
    [ "div", "span", "p", "section", "ul", "li", "a", "img", "button"
    , "table", "thead", "tbody", "tr", "td", "form", "input", "br" ])
  case lookupSpec t of
    Just s  => name s === t
    Nothing => failWith Nothing ("missing: " ++ t)

||| Catalog is internally consistent: every entry's `isVoid` flag agrees
||| with its `childPolicy = NoChildren`.
export
pbt_void_iff_no_children : Property
pbt_void_iff_no_children = withTests 1 . property $ do
  for_ elements $ \s => do
    let isVoidByFlag   = isVoid s
        isVoidByPolicy = case childPolicy s of
          NoChildren => True
          _          => False
    -- Every void element has NoChildren policy. (The converse need not
    -- hold: e.g. `<title>` is non-void TextOnly.)
    when isVoidByFlag $ isVoidByPolicy === True

||| Catalog tag-closure invariant (I2 lifted to a checked Idris proof).
||| The ingestion script asserts this in TypeScript; this test asserts
||| the same property at module-load time on `Cribrum.Html.Model.elements`
||| itself, so any future drift between the TS check and the emitted
||| catalog is caught here.
export
ext_catalog_child_tags_closed : Property
ext_catalog_child_tags_closed = oneShot $
  case decAllChildTagsExist elements of
    Yes _   => success
    No  _   => failWith Nothing "elements catalog contains an OnlyTags reference to a tag not in the catalog"

||| Sanity check on the decision procedure itself: a catalog with an
||| `OnlyTags` reference to a non-existent tag must be rejected. Guards
||| against mutants that collapse `decAllChildTagsExist` to a trivial
||| `Yes`.
export
ext_invariants_reject_bogus_tag : Property
ext_invariants_reject_bogus_tag = oneShot $
  let bogus : List ElementSpec
      bogus = [MkElementSpec "x" False False [] (OnlyTags ["nope"] False) []]
  in case decAllChildTagsExist bogus of
       Yes _ => failWith Nothing "decAllChildTagsExist should reject OnlyTags pointing to an absent tag"
       No  _ => success

export
group : Group
group = MkGroup "Cribrum.Html.Model"
  [ ("ext_lookup_div_present",                ext_lookup_div_present)
  , ("ext_lookup_unknown_absent",             ext_lookup_unknown_absent)
  , ("ext_void_set_has_br_img_hr",            ext_void_set_has_br_img_hr)
  , ("ext_html_only_admits_head_body",        ext_html_only_admits_head_body)
  , ("ext_p_is_phrasing_only",                ext_p_is_phrasing_only)
  , ("ext_ul_admits_li_template_script",      ext_ul_admits_li_template_script)
  , ("ext_categories_h1_includes_heading",    ext_categories_h1_includes_heading)
  , ("ext_categories_span_includes_phrasing", ext_categories_span_includes_phrasing)
  , ("ext_categories_unknown_empty",          ext_categories_unknown_empty)
  , ("ext_global_attr_class_recognized_on_any", ext_global_attr_class_recognized_on_any)
  , ("ext_data_attr_recognized",              ext_data_attr_recognized)
  , ("ext_aria_attr_recognized",              ext_aria_attr_recognized)
  , ("ext_on_event_attr_recognized",          ext_on_event_attr_recognized)
  , ("ext_href_attr_only_on_a_and_friends",   ext_href_attr_only_on_a_and_friends)
  , ("pddt_void_set_matches_html5",           pddt_void_set_matches_html5)
  , ("pddt_attr_allowance_table",             pddt_attr_allowance_table)
  , ("pbt_lookup_round_trip",                 pbt_lookup_round_trip)
  , ("pbt_void_iff_no_children",              pbt_void_iff_no_children)
  , ("ext_catalog_child_tags_closed",         ext_catalog_child_tags_closed)
  , ("ext_invariants_reject_bogus_tag",       ext_invariants_reject_bogus_tag)
  ]
