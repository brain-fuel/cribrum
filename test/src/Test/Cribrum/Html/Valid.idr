module Test.Cribrum.Html.Valid

import Data.List
import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.Html.Category
import Cribrum.Html.Model
import Cribrum.Html.Valid
import Test.Cribrum.Gen

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs — leaf validity.
--------------------------------------------------------------------------------

export
ext_text_is_valid : Property
ext_text_is_valid = oneShot $
  isValidHtml (Text "hi") === True

export
ext_comment_is_valid : Property
ext_comment_is_valid = oneShot $
  isValidHtml (Comment "x") === True

--------------------------------------------------------------------------------
-- EXTs — tag membership.
--------------------------------------------------------------------------------

export
ext_known_empty_element_valid : Property
ext_known_empty_element_valid = oneShot $
  isValidHtml (Element "p" [] []) === True

export
ext_paragraph_with_text_valid : Property
ext_paragraph_with_text_valid = oneShot $
  isValidHtml (Element "p" [] [Text "hello"]) === True

export
ext_unknown_tag_invalid : Property
ext_unknown_tag_invalid = oneShot $
  isValidHtml (Element "bogus" [] []) === False

||| Validity recurses: a known parent with an unknown child is invalid.
export
ext_unknown_child_invalidates_parent : Property
ext_unknown_child_invalidates_parent = oneShot $
  isValidHtml (Element "div" [] [Element "bogus" [] []]) === False

||| Multi-level nesting of known tags is valid.
export
ext_deeply_nested_known_valid : Property
ext_deeply_nested_known_valid = oneShot $
  isValidHtml
    (Element "section" []
      [ Element "article" []
          [ Element "h1" [] [Text "title"]
          , Element "p"  [] [Text "body"]
          ]
      ])
    === True

||| Deep nesting hides an unknown leaf — still rejected.
export
ext_deeply_nested_with_bogus_invalid : Property
ext_deeply_nested_with_bogus_invalid = oneShot $
  isValidHtml
    (Element "section" []
      [ Element "article" []
          [ Element "h1" [] [Text "title"]
          , Element "p"  [] [Element "bogus" [] []]
          ]
      ])
    === False

||| Decision procedure produces a proof we can pattern-match against.
export
ext_dec_produces_proof : Property
ext_dec_produces_proof = oneShot $
  case decideHtml (Element "p" [] [Text "hi"]) of
    Yes _ => success
    No  _ => failWith Nothing "expected Yes"

||| And for a rejection we get a refutation, not a partial result.
export
ext_dec_produces_refutation : Property
ext_dec_produces_refutation = oneShot $
  case decideHtml (Element "bogus" [] []) of
    Yes _ => failWith Nothing "expected No"
    No  _ => success

--------------------------------------------------------------------------------
-- EXTs — content model (P2.2).
--------------------------------------------------------------------------------

||| `<ul>` admits only `<li>` (plus `<script>` / `<template>`); a stray
||| `<p>` is rejected.
export
ext_ul_rejects_non_li_child : Property
ext_ul_rejects_non_li_child = oneShot $
  isValidHtml (Element "ul" [] [Element "p" [] []]) === False

||| `<ul>` populated only with `<li>` validates.
export
ext_ul_with_li_valid : Property
ext_ul_with_li_valid = oneShot $
  isValidHtml (Element "ul" [] [Element "li" [] [Text "item"]]) === True

||| Block-in-phrasing: `<p>` is phrasing-only; a nested `<div>` (flow,
||| not phrasing) is rejected.
export
ext_block_in_phrasing_rejected : Property
ext_block_in_phrasing_rejected = oneShot $
  isValidHtml (Element "p" [] [Element "div" [] []]) === False

||| Phrasing-in-phrasing: `<p>` accepts `<em>` (Phrasing).
export
ext_phrasing_in_phrasing_valid : Property
ext_phrasing_in_phrasing_valid = oneShot $
  isValidHtml (Element "p" [] [Element "em" [] [Text "x"]]) === True

||| Phrasing-in-flow: `<div>` (flow) accepts `<span>` (phrasing) by
||| subsumption.
export
ext_phrasing_in_flow_valid : Property
ext_phrasing_in_flow_valid = oneShot $
  isValidHtml (Element "div" [] [Element "span" [] [Text "x"]]) === True

||| Void elements reject any children.
export
ext_void_rejects_children : Property
ext_void_rejects_children = oneShot $
  isValidHtml (Element "br" [] [Text "x"]) === False

||| Void elements without children are valid.
export
ext_void_empty_valid : Property
ext_void_empty_valid = oneShot $
  isValidHtml (Element "br" [] []) === True

||| `<table>` only admits its structural children, not bare flow content.
export
ext_table_rejects_p_directly : Property
ext_table_rejects_p_directly = oneShot $
  isValidHtml (Element "table" [] [Element "p" [] []]) === False

||| `<table> > <tbody> > <tr> > <td>` validates.
export
ext_table_well_formed_valid : Property
ext_table_well_formed_valid = oneShot $
  isValidHtml
    (Element "table" []
      [ Element "tbody" []
          [ Element "tr" []
              [ Element "td" [] [Text "cell"] ]
          ]
      ])
    === True

||| `<head>` does not admit text content.
export
ext_head_rejects_text : Property
ext_head_rejects_text = oneShot $
  isValidHtml (Element "head" [] [Text "stuff"]) === False

||| `<head>` admits `<title>` (metadata).
export
ext_head_with_title_valid : Property
ext_head_with_title_valid = oneShot $
  isValidHtml (Element "head" [] [Element "title" [] [Text "doc"]]) === True

--------------------------------------------------------------------------------
-- EXTs — attribute permission.
--------------------------------------------------------------------------------

||| Global attribute `class` works on every element.
export
ext_global_attr_class_on_div : Property
ext_global_attr_class_on_div = oneShot $
  isValidHtml (Element "div" [MkHAttr "class" (Str "x")] []) === True

||| Local attribute `href` works on `<a>`.
export
ext_local_attr_href_on_a : Property
ext_local_attr_href_on_a = oneShot $
  isValidHtml (Element "a" [MkHAttr "href" (Str "/x")] []) === True

||| `<div>` does not accept `href` (only `<a>`, `<link>`, `<area>`, `<base>`).
export
ext_unknown_attr_on_div_rejected : Property
ext_unknown_attr_on_div_rejected = oneShot $
  isValidHtml (Element "div" [MkHAttr "href" (Str "/x")] []) === False

||| `data-*` attributes are accepted on every element.
export
ext_data_attr_anywhere : Property
ext_data_attr_anywhere = oneShot $
  isValidHtml (Element "section" [MkHAttr "data-thing" (Str "yes")] []) === True

||| `aria-*` attributes are accepted on every element.
export
ext_aria_attr_anywhere : Property
ext_aria_attr_anywhere = oneShot $
  isValidHtml (Element "button" [MkHAttr "aria-label" (Str "go")] []) === True

||| `on*` event-handler attributes are accepted on every element.
export
ext_on_event_attr_anywhere : Property
ext_on_event_attr_anywhere = oneShot $
  isValidHtml (Element "button" [MkHAttr "onclick" (Str "f()")] []) === True

--------------------------------------------------------------------------------
-- EXTs — located rejection (P2.3).
--------------------------------------------------------------------------------

||| Unknown root tag → `UnknownTag` at the root path.
export
ext_located_unknown_root : Property
ext_located_unknown_root = oneShot $
  case decideHtmlLocated (Element "bogus" [] []) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      UnknownTag "bogus" => path lr === []
      r => failWith Nothing ("expected UnknownTag, got " ++ show r)

||| Unknown child → `UnknownTag` with path pointing to that child.
export
ext_located_unknown_nested : Property
ext_located_unknown_nested = oneShot $
  case decideHtmlLocated
         (Element "section" []
           [ Element "p" []  [Text "a"]
           , Element "div" [] [Element "bogus" [] []]
           ]) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      UnknownTag "bogus" => path lr === [1, 0]
      r => failWith Nothing ("expected UnknownTag bogus, got " ++ show r)

||| `<p>` with a `<div>` child → `BlockInPhrasing` reason.
export
ext_located_block_in_phrasing : Property
ext_located_block_in_phrasing = oneShot $
  case decideHtmlLocated (Element "p" [] [Element "div" [] []]) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      BlockInPhrasing "p" "div" => path lr === [0]
      r => failWith Nothing ("expected BlockInPhrasing, got " ++ show r)

||| `<ul>` with a `<p>` child → `MalformedTable` (the `OnlyTags`-policy
||| diagnosis flavour; the name reflects table-style failures but it
||| applies to any `OnlyTags` parent — ul, ol, dl, table, tr, ...).
export
ext_located_ul_with_non_li : Property
ext_located_ul_with_non_li = oneShot $
  case decideHtmlLocated (Element "ul" [] [Element "p" [] []]) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      MalformedTable "ul" "p" => path lr === [0]
      r => failWith Nothing ("expected MalformedTable ul p, got " ++ show r)

||| Disallowed attribute → `DisallowedAttr` with the offending name.
export
ext_located_disallowed_attr : Property
ext_located_disallowed_attr = oneShot $
  case decideHtmlLocated (Element "div" [MkHAttr "href" (Str "/x")] []) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      DisallowedAttr "div" "href" => path lr === []
      r => failWith Nothing ("expected DisallowedAttr div href, got " ++ show r)

||| Bare text under `<ul>` → `TextNotAllowedIn`.
export
ext_located_text_in_ul : Property
ext_located_text_in_ul = oneShot $
  case decideHtmlLocated (Element "ul" [] [Text "stray"]) of
    Right _ => failWith Nothing "expected rejection"
    Left lr => case reason lr of
      TextNotAllowedIn "ul" => path lr === [0]
      r => failWith Nothing ("expected TextNotAllowedIn ul, got " ++ show r)

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

validityCases : List (Bool, HExpr)
validityCases =
  [ (True,  Text "")
  , (True,  Comment "c")
  , (True,  Element "html" [] [])
  , (True,  Element "p" [] [])
  , (True,  Element "div" [] [Text "x", Element "span" [] []])
  , (True,  Element "ul" [] [Element "li" [] [Text "1"]])
  , (False, Element "marquee" [] [])
  , (False, Element "p" [] [Element "blink" [] []])
  , (False, Element "DIV" [] [])                -- case-sensitive
  , (False, Element "" [] [])                   -- empty tag rejected
  , (False, Element "section" []
              [ Element "article" []
                  [ Element "wat" [] [] ]
              ])
  -- Phase 2 additions: content-model rejections.
  , (False, Element "p"     [] [Element "div" [] []])         -- block-in-phrasing
  , (False, Element "ul"    [] [Element "p" [] []])           -- non-li under ul
  , (False, Element "br"    [] [Text "x"])                    -- children under void
  , (False, Element "table" [] [Element "p" [] []])           -- bad table child
  , (False, Element "head"  [] [Text "x"])                    -- text under head
  -- Phase 2 additions: attribute rejections.
  , (False, Element "div"   [MkHAttr "href" (Str "/x")] [])   -- href not on div
  , (True,  Element "a"     [MkHAttr "href" (Str "/x")] [])   -- href on a
  , (True,  Element "div"   [MkHAttr "data-x" (Str "y")] []) -- data-* anywhere
  ]

export
pddt_validity_table : Property
pddt_validity_table = withTests 1 . property $ do
  for_ validityCases $ \(expected, tree) =>
    isValidHtml tree === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| `decideHtml` is total: it always returns Yes or No (i.e. never crashes).
||| Encoded as: `isValidHtml` reduces to a Bool — observing it is enough.
export
pbt_dec_total : Property
pbt_dec_total = property $ do
  h <- forAll hexpr
  let b = isValidHtml h
  diff b (\_,_ => True) b

||| Soundness: any tree built by `genValidTree` validates by construction.
||| Replaces the spike-era `pbt_known_trees_validate` which only respected
||| tag membership; the content-aware generator now also respects per-
||| element child placement.
export
pbt_constructed_valid_trees_validate : Property
pbt_constructed_valid_trees_validate = property $ do
  h <- forAll (genValidTree 3)
  isValidHtml h === True

||| Completeness at the root: replacing the root tag with a non-catalog
||| string always rejects, regardless of the (arbitrary) children below.
unknownTagsGen : Gen String
unknownTagsGen = element $ the (Vect _ String)
  [ "bogus", "marquee", "blink", "wat", "frobnicate", "xx" ]

export
pbt_unknown_root_tag_invalidates : Property
pbt_unknown_root_tag_invalidates = property $ do
  cs  <- forAll (list (linear 0 3) (genFlowTree 2))
  tag <- forAll unknownTagsGen
  isValidHtml (Element tag [] cs) === False

||| `decideHtmlLocated` agrees with `decideHtml`: a tree validates under
||| `decideHtml` iff it returns `Right` under `decideHtmlLocated`.
export
pbt_located_agrees_with_dec : Property
pbt_located_agrees_with_dec = property $ do
  h <- forAll hexpr
  let validDec  = isValidHtml h
      validLoc  = case decideHtmlLocated h of
        Right _ => True
        Left _  => False
  validDec === validLoc

||| Phrasing-content soundness: phrasing trees fit inside `<p>`.
export
pbt_phrasing_fits_in_p : Property
pbt_phrasing_fits_in_p = property $ do
  cs <- forAll (list (linear 0 4) (genPhrasingTree 2))
  isValidHtml (Element "p" [] cs) === True

export
group : Group
group = MkGroup "Cribrum.Html.Valid"
  [ ("ext_text_is_valid",                       ext_text_is_valid)
  , ("ext_comment_is_valid",                    ext_comment_is_valid)
  , ("ext_known_empty_element_valid",           ext_known_empty_element_valid)
  , ("ext_paragraph_with_text_valid",           ext_paragraph_with_text_valid)
  , ("ext_unknown_tag_invalid",                 ext_unknown_tag_invalid)
  , ("ext_unknown_child_invalidates_parent",    ext_unknown_child_invalidates_parent)
  , ("ext_deeply_nested_known_valid",           ext_deeply_nested_known_valid)
  , ("ext_deeply_nested_with_bogus_invalid",    ext_deeply_nested_with_bogus_invalid)
  , ("ext_dec_produces_proof",                  ext_dec_produces_proof)
  , ("ext_dec_produces_refutation",             ext_dec_produces_refutation)
  , ("ext_ul_rejects_non_li_child",             ext_ul_rejects_non_li_child)
  , ("ext_ul_with_li_valid",                    ext_ul_with_li_valid)
  , ("ext_block_in_phrasing_rejected",          ext_block_in_phrasing_rejected)
  , ("ext_phrasing_in_phrasing_valid",          ext_phrasing_in_phrasing_valid)
  , ("ext_phrasing_in_flow_valid",              ext_phrasing_in_flow_valid)
  , ("ext_void_rejects_children",               ext_void_rejects_children)
  , ("ext_void_empty_valid",                    ext_void_empty_valid)
  , ("ext_table_rejects_p_directly",            ext_table_rejects_p_directly)
  , ("ext_table_well_formed_valid",             ext_table_well_formed_valid)
  , ("ext_head_rejects_text",                   ext_head_rejects_text)
  , ("ext_head_with_title_valid",               ext_head_with_title_valid)
  , ("ext_global_attr_class_on_div",            ext_global_attr_class_on_div)
  , ("ext_local_attr_href_on_a",                ext_local_attr_href_on_a)
  , ("ext_unknown_attr_on_div_rejected",        ext_unknown_attr_on_div_rejected)
  , ("ext_data_attr_anywhere",                  ext_data_attr_anywhere)
  , ("ext_aria_attr_anywhere",                  ext_aria_attr_anywhere)
  , ("ext_on_event_attr_anywhere",              ext_on_event_attr_anywhere)
  , ("ext_located_unknown_root",                ext_located_unknown_root)
  , ("ext_located_unknown_nested",              ext_located_unknown_nested)
  , ("ext_located_block_in_phrasing",           ext_located_block_in_phrasing)
  , ("ext_located_ul_with_non_li",              ext_located_ul_with_non_li)
  , ("ext_located_disallowed_attr",             ext_located_disallowed_attr)
  , ("ext_located_text_in_ul",                  ext_located_text_in_ul)
  , ("pddt_validity_table",                     pddt_validity_table)
  , ("pbt_dec_total",                           pbt_dec_total)
  , ("pbt_constructed_valid_trees_validate",    pbt_constructed_valid_trees_validate)
  , ("pbt_unknown_root_tag_invalidates",        pbt_unknown_root_tag_invalidates)
  , ("pbt_located_agrees_with_dec",             pbt_located_agrees_with_dec)
  , ("pbt_phrasing_fits_in_p",                  pbt_phrasing_fits_in_p)
  ]
