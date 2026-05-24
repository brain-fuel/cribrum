module Test.Cribrum.AA.Typed

import Data.List.Quantifiers
import Data.So
import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.AA.Typed

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_text_node_ok : Property
ext_text_node_ok = oneShot $
  imgsAllOk (Text "x") === True

export
ext_p_with_text_ok : Property
ext_p_with_text_ok = oneShot $
  imgsAllOk (Element "p" [] [Text "hi"]) === True

export
ext_img_with_alt_ok : Property
ext_img_with_alt_ok = oneShot $
  imgsAllOk
    (Element "img"
       [ MkHAttr "src" (Str "/x")
       , MkHAttr "alt" (Str "cat")
       ] []) === True

export
ext_img_without_alt_fails : Property
ext_img_without_alt_fails = oneShot $
  imgsAllOk (Element "img" [MkHAttr "src" (Str "/x")] []) === False

||| Failure is recursive: img nested deep without alt fails the whole-tree
||| proposition.
export
ext_nested_img_without_alt_fails : Property
ext_nested_img_without_alt_fails = oneShot $
  imgsAllOk
    (Element "section" []
       [ Element "p" [] [Text "ok"]
       , Element "figure" []
           [ Element "img" [] []        -- offender
           , Element "figcaption" [] [Text "x"]
           ]
       ]) === False

||| A nested img WITH alt is ok even if its parent has no alt rules.
export
ext_nested_img_with_alt_ok : Property
ext_nested_img_with_alt_ok = oneShot $
  imgsAllOk
    (Element "section" []
       [ Element "figure" []
           [ Element "img" [MkHAttr "alt" (Str "ok")] []
           , Element "figcaption" [] [Text "x"]
           ]
       ]) === True

||| Decision returns Yes for ok trees: the witness `(h ** ImgsAllOk h)` is
||| constructable — Phase 4's whole point.
export
ext_dec_yes_for_ok_tree : Property
ext_dec_yes_for_ok_tree = oneShot $
  case decImgsAllOk (Element "img" [MkHAttr "alt" (Str "a")] []) of
    Yes _ => success
    No  _ => failWith Nothing "expected Yes"

||| Decision returns No (a real refutation) for bad trees.
export
ext_dec_no_for_bad_tree : Property
ext_dec_no_for_bad_tree = oneShot $
  case decImgsAllOk (Element "img" [] []) of
    Yes _ => failWith Nothing "expected No"
    No  _ => success

||| Non-img element with NO alt attribute passes — the rule fires only on
||| `<img>` (`img-alt` is not `every-element-must-have-alt`).
export
ext_div_without_alt_ok : Property
ext_div_without_alt_ok = oneShot $
  imgsAllOk (Element "div" [] []) === True

||| Attribute lookup is exact: `altText` (close but not `alt`) does NOT
||| satisfy the rule.
export
ext_close_attr_name_does_not_satisfy : Property
ext_close_attr_name_does_not_satisfy = oneShot $
  imgsAllOk
    (Element "img" [MkHAttr "altText" (Str "x")] []) === False

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

cases : List (Bool, HExpr)
cases =
  [ (True,  Text "x")
  , (True,  Comment "x")
  , (True,  Element "p" [] [])
  , (True,  Element "div" [MkHAttr "id" (Str "x")] [])
  , (True,  Element "img" [MkHAttr "alt" (Str "ok")] [])
  -- empty alt still counts as "has alt attribute" (the *meaningful* check
  -- is the heuristic rule alt-meaningful, not this structural one)
  , (True,  Element "img" [MkHAttr "alt" (Str "")] [])
  , (False, Element "img" [] [])
  , (False, Element "img" [MkHAttr "src" (Str "/x.png")] [])
  -- multiple imgs: ALL must satisfy
  , (False, Element "section" []
              [ Element "img" [MkHAttr "alt" (Str "ok")] []
              , Element "img" [] []                              -- offender
              ])
  , (True,  Element "section" []
              [ Element "img" [MkHAttr "alt" (Str "a")] []
              , Element "img" [MkHAttr "alt" (Str "b")] []
              ])
  ]

export
pddt_imgsAllOk_table : Property
pddt_imgsAllOk_table = withTests 1 . property $ do
  for_ cases $ \(expected, tree) =>
    imgsAllOk tree === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| A tree containing no `<img>` element trivially passes the rule.
nonImgTag : Gen String
nonImgTag = element $ the (Vect _ String)
  ["p", "div", "span", "section", "article", "nav", "ul", "li", "a"]

nonImgTree : (depthBudget : Nat) -> Gen HExpr
nonImgTree 0 = Text <$> string (linear 0 8) ascii
nonImgTree (S k) = choice $ the (Vect _ (Gen HExpr))
  [ Text <$> string (linear 0 8) ascii
  , [| Element nonImgTag (pure []) (list (linear 0 3) (nonImgTree k)) |]
  ]

export
pbt_no_img_tree_always_ok : Property
pbt_no_img_tree_always_ok = property $ do
  h <- forAll (nonImgTree 3)
  imgsAllOk h === True

||| A tree that contains at least one bare `<img>` is always not-ok.
treeWithBareImg : Gen HExpr
treeWithBareImg = do
  bare <- pure (Element "img" [MkHAttr "src" (Str "/x")] [])
  wraps <- nat (constant 0 4)
  let wrap : Nat -> HExpr -> HExpr
      wrap 0     h = h
      wrap (S k) h = Element "div" [] [wrap k h]
  pure (wrap wraps bare)

export
pbt_tree_with_bare_img_always_fails : Property
pbt_tree_with_bare_img_always_fails = property $ do
  h <- forAll treeWithBareImg
  imgsAllOk h === False

||| The decision procedure is total (never diverges); observe via boolean
||| projection.
export
pbt_decision_total : Property
pbt_decision_total = property $ do
  h <- forAll (nonImgTree 3)
  let b = imgsAllOk h
  diff b (\_,_ => True) b

||| Soundness/agreement: `imgsAllOk` (bool) agrees with `decImgsAllOk` (Dec).
export
pbt_bool_agrees_with_dec : Property
pbt_bool_agrees_with_dec = property $ do
  h <- forAll (nonImgTree 3)
  case decImgsAllOk h of
    Yes _ => imgsAllOk h === True
    No  _ => imgsAllOk h === False

--------------------------------------------------------------------------------
-- anchor-href tests (mirrors img-alt; second rule demonstrates generalisation).
--------------------------------------------------------------------------------

export
ext_anchor_with_href_ok : Property
ext_anchor_with_href_ok = oneShot $
  anchorsAllOk (Element "a" [MkHAttr "href" (Str "/x")] [Text "go"]) === True

export
ext_anchor_without_href_fails : Property
ext_anchor_without_href_fails = oneShot $
  anchorsAllOk (Element "a" [] [Text "go"]) === False

export
ext_nested_anchor_without_href_fails : Property
ext_nested_anchor_without_href_fails = oneShot $
  anchorsAllOk
    (Element "section" []
       [ Element "p" [] [Element "a" [] [Text "x"]] ]) === False

anchorCases : List (Bool, HExpr)
anchorCases =
  [ (True,  Text "x")
  , (True,  Element "p" [] [])
  , (True,  Element "a" [MkHAttr "href" (Str "/x")] [Text "go"])
  , (False, Element "a" [] [Text "go"])
  , (False, Element "a" [MkHAttr "title" (Str "x")] [Text "go"])
  ]

export
pddt_anchorsAllOk_table : Property
pddt_anchorsAllOk_table = withTests 1 . property $ do
  for_ anchorCases $ \(expected, tree) =>
    anchorsAllOk tree === expected

export
pbt_anchor_with_href_always_ok : Property
pbt_anchor_with_href_always_ok = property $ do
  href <- forAll (string (linear 1 16) ascii)
  body <- forAll (string (linear 0 8) ascii)
  anchorsAllOk
    (Element "a" [MkHAttr "href" (Str href)] [Text body]) === True

--------------------------------------------------------------------------------
-- iframe-title tests.
--------------------------------------------------------------------------------

export
ext_iframe_with_title_typed_ok : Property
ext_iframe_with_title_typed_ok = oneShot $
  iframesAllOk
    (Element "iframe" [ MkHAttr "src"   (Str "/x")
                      , MkHAttr "title" (Str "preview") ] []) === True

export
ext_iframe_without_title_typed_fails : Property
ext_iframe_without_title_typed_fails = oneShot $
  iframesAllOk
    (Element "iframe" [MkHAttr "src" (Str "/x")] []) === False

export
ext_iframe_with_empty_title_typed_fails : Property
ext_iframe_with_empty_title_typed_fails = oneShot $
  iframesAllOk
    (Element "iframe" [MkHAttr "title" (Str "   ")] []) === False

export
ext_nested_iframe_without_title_typed_fails : Property
ext_nested_iframe_without_title_typed_fails = oneShot $
  iframesAllOk
    (Element "section" []
       [ Element "p" [] [Text "before"]
       , Element "iframe" [MkHAttr "src" (Str "/x")] []
       ]) === False

export
ext_no_iframe_tree_typed_ok : Property
ext_no_iframe_tree_typed_ok = oneShot $
  iframesAllOk
    (Element "section" []
       [ Element "p" [] [Text "ok"]
       , Element "img" [MkHAttr "alt" (Str "x")] []
       ]) === True

--------------------------------------------------------------------------------
-- label-for-control tests.
--------------------------------------------------------------------------------

export
ext_label_with_for_typed_ok : Property
ext_label_with_for_typed_ok = oneShot $
  labelsAllOk
    (Element "label" [MkHAttr "for" (Str "x")] [Text "name"]) === True

export
ext_label_with_implicit_input_typed_ok : Property
ext_label_with_implicit_input_typed_ok = oneShot $
  labelsAllOk
    (Element "label" []
       [ Text "name"
       , Element "input" [MkHAttr "type" (Str "text")] []
       ]) === True

export
ext_orphan_label_typed_fails : Property
ext_orphan_label_typed_fails = oneShot $
  labelsAllOk (Element "label" [] [Text "orphan"]) === False

export
ext_label_with_empty_for_typed_fails : Property
ext_label_with_empty_for_typed_fails = oneShot $
  labelsAllOk
    (Element "label" [MkHAttr "for" (Str "")] [Text "x"]) === False

--------------------------------------------------------------------------------
-- button-name tests.
--------------------------------------------------------------------------------

export
ext_button_with_text_typed_ok : Property
ext_button_with_text_typed_ok = oneShot $
  buttonsAllOk (Element "button" [] [Text "Submit"]) === True

export
ext_button_with_aria_label_typed_ok : Property
ext_button_with_aria_label_typed_ok = oneShot $
  buttonsAllOk
    (Element "button" [MkHAttr "aria-label" (Str "go")] []) === True

export
ext_empty_button_typed_fails : Property
ext_empty_button_typed_fails = oneShot $
  buttonsAllOk (Element "button" [] []) === False

export
ext_whitespace_button_typed_fails : Property
ext_whitespace_button_typed_fails = oneShot $
  buttonsAllOk (Element "button" [] [Text "   "]) === False

export
pbt_button_with_aria_label_always_ok : Property
pbt_button_with_aria_label_always_ok = property $ do
  lbl <- forAll (string (linear 1 16) ascii)
  buttonsAllOk
    (Element "button" [MkHAttr "aria-label" (Str (lbl ++ "x"))] []) === True

export
group : Group
group = MkGroup "Cribrum.AA.Typed"
  [ ("ext_text_node_ok",                       ext_text_node_ok)
  , ("ext_p_with_text_ok",                     ext_p_with_text_ok)
  , ("ext_img_with_alt_ok",                    ext_img_with_alt_ok)
  , ("ext_img_without_alt_fails",              ext_img_without_alt_fails)
  , ("ext_nested_img_without_alt_fails",       ext_nested_img_without_alt_fails)
  , ("ext_nested_img_with_alt_ok",             ext_nested_img_with_alt_ok)
  , ("ext_dec_yes_for_ok_tree",                ext_dec_yes_for_ok_tree)
  , ("ext_dec_no_for_bad_tree",                ext_dec_no_for_bad_tree)
  , ("ext_div_without_alt_ok",                 ext_div_without_alt_ok)
  , ("ext_close_attr_name_does_not_satisfy",   ext_close_attr_name_does_not_satisfy)
  , ("pddt_imgsAllOk_table",                   pddt_imgsAllOk_table)
  , ("pbt_no_img_tree_always_ok",              pbt_no_img_tree_always_ok)
  , ("pbt_tree_with_bare_img_always_fails",    pbt_tree_with_bare_img_always_fails)
  , ("pbt_decision_total",                     pbt_decision_total)
  , ("pbt_bool_agrees_with_dec",               pbt_bool_agrees_with_dec)
  , ("ext_anchor_with_href_ok",                ext_anchor_with_href_ok)
  , ("ext_anchor_without_href_fails",          ext_anchor_without_href_fails)
  , ("ext_nested_anchor_without_href_fails",   ext_nested_anchor_without_href_fails)
  , ("pddt_anchorsAllOk_table",                pddt_anchorsAllOk_table)
  , ("pbt_anchor_with_href_always_ok",         pbt_anchor_with_href_always_ok)
  , ("ext_iframe_with_title_typed_ok",         ext_iframe_with_title_typed_ok)
  , ("ext_iframe_without_title_typed_fails",   ext_iframe_without_title_typed_fails)
  , ("ext_iframe_with_empty_title_typed_fails",ext_iframe_with_empty_title_typed_fails)
  , ("ext_nested_iframe_without_title_typed_fails", ext_nested_iframe_without_title_typed_fails)
  , ("ext_no_iframe_tree_typed_ok",            ext_no_iframe_tree_typed_ok)
  , ("ext_label_with_for_typed_ok",            ext_label_with_for_typed_ok)
  , ("ext_label_with_implicit_input_typed_ok", ext_label_with_implicit_input_typed_ok)
  , ("ext_orphan_label_typed_fails",           ext_orphan_label_typed_fails)
  , ("ext_label_with_empty_for_typed_fails",   ext_label_with_empty_for_typed_fails)
  , ("ext_button_with_text_typed_ok",          ext_button_with_text_typed_ok)
  , ("ext_button_with_aria_label_typed_ok",    ext_button_with_aria_label_typed_ok)
  , ("ext_empty_button_typed_fails",           ext_empty_button_typed_fails)
  , ("ext_whitespace_button_typed_fails",      ext_whitespace_button_typed_fails)
  , ("pbt_button_with_aria_label_always_ok",   pbt_button_with_aria_label_always_ok)
  ]
