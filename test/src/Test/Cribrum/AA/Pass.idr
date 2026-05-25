module Test.Cribrum.AA.Pass

import Data.List
import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.Html.Valid
import Cribrum.AA.Catalog
import Cribrum.AA.Pass

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

||| Lift a tree to a checked one. Asserts the tree is structurally valid HTML
||| so the precondition is real; if not the test fails loudly.
checked : HExpr -> Either String AAReport
checked h = case decideHtml h of
  Yes p => Right (checkAA h p)
  No  _ => Left "test setup error: tree is not valid HTML"

ruleIds : AAReport -> List String
ruleIds = map (\f => Rule.id (rule f))

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_valid_p_no_findings : Property
ext_valid_p_no_findings = oneShot $
  case checked (Element "p" [] [Text "hi"]) of
    Right report => report === []
    Left e       => failWith Nothing e

export
ext_img_no_alt_emits_finding : Property
ext_img_no_alt_emits_finding = oneShot $
  case checked (Element "img" [MkHAttr "src" (Str "/x.png")] []) of
    Right report =>
      ruleIds report === ["img-alt"]
    Left e => failWith Nothing e

export
ext_img_with_alt_no_structural_finding : Property
ext_img_with_alt_no_structural_finding = oneShot $
  case checked (Element "img"
                 [ MkHAttr "src" (Str "/x.png")
                 , MkHAttr "alt" (Str "a friendly cat")
                 ] []) of
    Right report => structuralFindings report === []
    Left e => failWith Nothing e

export
ext_anchor_no_href_emits_finding : Property
ext_anchor_no_href_emits_finding = oneShot $
  case checked (Element "a" [] [Text "click"]) of
    Right report => ruleIds report === ["anchor-href"]
    Left e => failWith Nothing e

export
ext_anchor_with_href_no_structural_finding : Property
ext_anchor_with_href_no_structural_finding = oneShot $
  case checked (Element "a" [MkHAttr "href" (Str "/x")] [Text "go"]) of
    Right report => structuralFindings report === []
    Left e => failWith Nothing e

||| Filename-looking alt text is a HEURISTIC (warning), not an error.
export
ext_alt_looks_like_filename_is_heuristic : Property
ext_alt_looks_like_filename_is_heuristic = oneShot $
  case checked (Element "img"
                 [ MkHAttr "src" (Str "/x.png")
                 , MkHAttr "alt" (Str "x.png")
                 ] []) of
    Right report => do
      -- Heuristic finding present.
      ruleIds (heuristicFindings report) === ["alt-meaningful"]
      -- No structural finding (alt IS present).
      structuralFindings report === []
    Left e => failWith Nothing e

export
ext_heading_skip_emits_finding : Property
ext_heading_skip_emits_finding = oneShot $
  case checked (Element "section" []
                 [ Element "h1" [] [Text "a"]
                 , Element "h3" [] [Text "skipped h2"]
                 ]) of
    Right report => ruleIds report === ["heading-no-skip"]
    Left e => failWith Nothing e

export
ext_heading_consecutive_ok : Property
ext_heading_consecutive_ok = oneShot $
  case checked (Element "section" []
                 [ Element "h1" [] [Text "a"]
                 , Element "h2" [] [Text "b"]
                 , Element "h3" [] [Text "c"]
                 ]) of
    Right report => structuralFindings report === []
    Left e => failWith Nothing e

||| Multiple findings of different rules co-exist.
export
ext_multiple_findings : Property
ext_multiple_findings = oneShot $
  case checked (Element "section" []
                 [ Element "img" [] []                       -- img-alt
                 , Element "a"   [] [Text "x"]               -- anchor-href
                 , Element "h1"  [] [Text "t"]
                 , Element "h3"  [] [Text "skip"]            -- heading-no-skip
                 ]) of
    Right report => sort (ruleIds report)
      === sort ["img-alt", "anchor-href", "heading-no-skip"]
    Left e => failWith Nothing e

||| Pass exposes per-finding path so callers can deep-link.
export
ext_finding_path_locates_node : Property
ext_finding_path_locates_node = oneShot $
  case checked (Element "section" []
                 [ Element "p"   [] [Text "ok"]
                 , Element "img" [] []          -- second child
                 ]) of
    Right [f] => path f === [1]
    Right xs  => failWith Nothing
                    ("expected single finding, got " ++ show xs)
    Left  e   => failWith Nothing e

-- unique-main EXTs.

export
ext_no_main_no_finding : Property
ext_no_main_no_finding = oneShot $
  case checked (Element "section" [] [Text "no landmark"]) of
    Right report => structuralFindings report === []
    Left e       => failWith Nothing e

export
ext_single_main_no_finding : Property
ext_single_main_no_finding = oneShot $
  case checked (Element "main" [] [Element "p" [] [Text "hi"]]) of
    Right report => structuralFindings report === []
    Left e       => failWith Nothing e

||| Two sibling `<main>` elements -> exactly one `unique-main` finding.
export
ext_two_sibling_mains_fires : Property
ext_two_sibling_mains_fires = oneShot $
  case checked (Element "div" []
                 [ Element "main" [] [Text "a"]
                 , Element "main" [] [Text "b"]
                 ]) of
    Right report =>
      ruleIds (structuralFindings report) === ["unique-main"]
    Left e => failWith Nothing e

||| Three `<main>` siblings still emits exactly one finding (whole-tree
||| rule fires once per tree, count surfaces in the finding message).
export
ext_three_mains_one_finding : Property
ext_three_mains_one_finding = oneShot $
  case checked (Element "div" []
                 [ Element "main" [] [Text "a"]
                 , Element "main" [] [Text "b"]
                 , Element "main" [] [Text "c"]
                 ]) of
    Right report =>
      ruleIds (structuralFindings report) === ["unique-main"]
    Left e => failWith Nothing e

--------------------------------------------------------------------------------
-- EXTs — new structural rules (document-lang, iframe-title, label-for,
-- fieldset-legend, link-name, button-name, duplicate-id).
--------------------------------------------------------------------------------

export
ext_html_root_without_lang_fires : Property
ext_html_root_without_lang_fires = oneShot $
  case checked (Element "html" [] [Element "body" [] [Text "."]]) of
    Right report => elem "document-lang" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_html_root_with_lang_ok : Property
ext_html_root_with_lang_ok = oneShot $
  case checked (Element "html" [MkHAttr "lang" (Str "en")]
                  [Element "body" [] [Text "."]]) of
    Right report => elem "document-lang" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_html_root_empty_lang_fires : Property
ext_html_root_empty_lang_fires = oneShot $
  case checked (Element "html" [MkHAttr "lang" (Str "")]
                  [Element "body" [] [Text "."]]) of
    Right report => elem "document-lang" (ruleIds report) === True
    Left e       => failWith Nothing e

||| `document-lang` is a root-only check; an inner `<html>` (impossible
||| in valid HTML, but covers the walker logic) doesn't fire when the
||| tree's root isn't html.
export
ext_document_lang_only_at_root : Property
ext_document_lang_only_at_root = oneShot $
  case checked (Element "div" [] [Element "p" [] [Text "."]]) of
    Right report => elem "document-lang" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_iframe_without_title_fires : Property
ext_iframe_without_title_fires = oneShot $
  case checked (Element "section" []
                  [Element "iframe" [MkHAttr "src" (Str "/x")] []]) of
    Right report => elem "iframe-title" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_iframe_with_title_ok : Property
ext_iframe_with_title_ok = oneShot $
  case checked (Element "section" []
                  [Element "iframe" [ MkHAttr "src"   (Str "/x")
                                    , MkHAttr "title" (Str "embedded preview")
                                    ] []]) of
    Right report => elem "iframe-title" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_label_with_for_ok : Property
ext_label_with_for_ok = oneShot $
  case checked (Element "form" []
                  [ Element "label" [MkHAttr "for" (Str "x")] [Text "name"]
                  , Element "input" [MkHAttr "id"  (Str "x")] []
                  ]) of
    Right report => elem "label-for-control" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_label_with_implicit_control_ok : Property
ext_label_with_implicit_control_ok = oneShot $
  case checked (Element "label" []
                  [ Text "name"
                  , Element "input" [MkHAttr "type" (Str "text")] []
                  ]) of
    Right report => elem "label-for-control" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_label_without_for_or_control_fires : Property
ext_label_without_for_or_control_fires = oneShot $
  case checked (Element "label" [] [Text "orphan"]) of
    Right report => elem "label-for-control" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_fieldset_without_legend_fires : Property
ext_fieldset_without_legend_fires = oneShot $
  case checked (Element "fieldset" []
                  [Element "input" [MkHAttr "type" (Str "text")] []]) of
    Right report => elem "fieldset-legend" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_fieldset_with_legend_ok : Property
ext_fieldset_with_legend_ok = oneShot $
  case checked (Element "fieldset" []
                  [ Element "legend" [] [Text "group"]
                  , Element "input"  [MkHAttr "type" (Str "text")] []
                  ]) of
    Right report => elem "fieldset-legend" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_link_without_text_fires : Property
ext_link_without_text_fires = oneShot $
  case checked (Element "a" [MkHAttr "href" (Str "/x")] []) of
    Right report => elem "link-name" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_link_with_text_ok : Property
ext_link_with_text_ok = oneShot $
  case checked (Element "a" [MkHAttr "href" (Str "/x")] [Text "click"]) of
    Right report => elem "link-name" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_link_with_aria_label_ok : Property
ext_link_with_aria_label_ok = oneShot $
  case checked (Element "a" [ MkHAttr "href"       (Str "/x")
                            , MkHAttr "aria-label" (Str "go")
                            ] []) of
    Right report => elem "link-name" (ruleIds report) === False
    Left e       => failWith Nothing e

||| `<a>` without `href` (e.g. JS-only anchor or in-page anchor target)
||| does not trigger link-name; it triggers `anchor-href` instead.
export
ext_anchor_without_href_skips_link_name : Property
ext_anchor_without_href_skips_link_name = oneShot $
  case checked (Element "a" [] []) of
    Right report => elem "link-name" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_button_without_text_fires : Property
ext_button_without_text_fires = oneShot $
  case checked (Element "button" [] []) of
    Right report => elem "button-name" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_button_with_text_ok : Property
ext_button_with_text_ok = oneShot $
  case checked (Element "button" [] [Text "Submit"]) of
    Right report => elem "button-name" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_duplicate_id_fires : Property
ext_duplicate_id_fires = oneShot $
  case checked (Element "section" []
                  [ Element "div" [MkHAttr "id" (Str "x")] []
                  , Element "p"   [MkHAttr "id" (Str "x")] [Text "."]
                  ]) of
    Right report => elem "duplicate-id" (ruleIds report) === True
    Left e       => failWith Nothing e

export
ext_unique_ids_ok : Property
ext_unique_ids_ok = oneShot $
  case checked (Element "section" []
                  [ Element "div" [MkHAttr "id" (Str "a")] []
                  , Element "p"   [MkHAttr "id" (Str "b")] [Text "."]
                  ]) of
    Right report => elem "duplicate-id" (ruleIds report) === False
    Left e       => failWith Nothing e

export
ext_triplicate_id_fires_twice : Property
ext_triplicate_id_fires_twice = oneShot $
  case checked (Element "section" []
                  [ Element "div" [MkHAttr "id" (Str "x")] []
                  , Element "p"   [MkHAttr "id" (Str "x")] [Text "."]
                  , Element "span" [MkHAttr "id" (Str "x")] [Text "."]
                  ]) of
    Right report =>
      length (filter (\f => Rule.id (rule f) == "duplicate-id") report) === 2
    Left e       => failWith Nothing e

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

||| Expected finding outcome for an `<img>` with various alt-attr states.
data AltExpect = ExpectStructural | ExpectHeuristic | ExpectNone

Eq AltExpect where
  ExpectStructural == ExpectStructural = True
  ExpectHeuristic  == ExpectHeuristic  = True
  ExpectNone       == ExpectNone       = True
  _                == _                = False

Show AltExpect where
  show ExpectStructural = "ExpectStructural"
  show ExpectHeuristic  = "ExpectHeuristic"
  show ExpectNone       = "ExpectNone"

altCases : List (Maybe String, AltExpect)
altCases =
  [ (Nothing,                   ExpectStructural)  -- missing -> structural
  , (Just "",                   ExpectHeuristic)   -- empty -> heuristic
  , (Just "x.png",              ExpectHeuristic)   -- filename
  , (Just "y.JPG",              ExpectHeuristic)
  , (Just "a real description", ExpectNone)
  ]

attrsWithAlt : Maybe String -> List HAttr
attrsWithAlt Nothing  = [MkHAttr "src" (Str "/x")]
attrsWithAlt (Just s) = [MkHAttr "src" (Str "/x"), MkHAttr "alt" (Str s)]

export
pddt_alt_findings : Property
pddt_alt_findings = withTests 1 . property $ do
  for_ altCases $ \(maybeAlt, expect) =>
    case checked (Element "img" (attrsWithAlt maybeAlt) []) of
      Right report => case expect of
        ExpectStructural =>
          diff (length (structuralFindings report)) (>=) 1
        ExpectHeuristic => do
          length (structuralFindings report) === 0
          diff (length (heuristicFindings report)) (>=) 1
        ExpectNone =>
          report === []
      Left e => failWith Nothing e

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| Generate a tree that has at least one `img` with no `alt`. The report
||| MUST contain at least one img-alt finding.
imgWithoutAlt : Gen HExpr
imgWithoutAlt = pure (Element "img" [MkHAttr "src" (Str "/x")] [])

wrapInDiv : Gen HExpr -> Gen HExpr
wrapInDiv g = (\h => Element "div" [] [h]) <$> g

export
pbt_img_without_alt_always_reported : Property
pbt_img_without_alt_always_reported = property $ do
  -- Possibly wrap in a div N times.
  depth <- forAll (nat $ constant 0 4)
  let inner = foldr (\_,g => wrapInDiv g) imgWithoutAlt (List.replicate depth ())
  h <- forAll inner
  case checked h of
    Right r =>
      diff (length (filter (\f => Rule.id (rule f) == "img-alt") r)) (>=) 1
    Left e  => failWith Nothing e

||| A leaf tree of only text or comment never emits AA findings.
export
pbt_text_only_tree_no_findings : Property
pbt_text_only_tree_no_findings = property $ do
  s <- forAll (string (linear 0 16) ascii)
  case checked (Text s) of
    Right r => r === []
    Left e  => failWith Nothing e

||| `checkAA` is total: it always returns a list (possibly empty), never
||| diverges. Observed by running on arbitrary valid trees built from
||| tags whose content policy admits text/phrasing children directly so
||| `decideHtml` produces a Yes (Phase 2's content-model check applies).
export
pbt_checkAA_total : Property
pbt_checkAA_total = property $ do
  let knownGen = element $ the (Vect _ String)
        ["p", "div", "span", "section", "h1", "h2"]
  tag <- forAll knownGen
  cs  <- forAll (list (linear 0 4)
                   ((\s => Text s) <$> string (linear 0 8) ascii))
  case checked (Element tag [] cs) of
    Right _ => success
    Left e  => failWith Nothing e

export
group : Group
group = MkGroup "Cribrum.AA.Pass"
  [ ("ext_valid_p_no_findings",                ext_valid_p_no_findings)
  , ("ext_img_no_alt_emits_finding",           ext_img_no_alt_emits_finding)
  , ("ext_img_with_alt_no_structural_finding", ext_img_with_alt_no_structural_finding)
  , ("ext_anchor_no_href_emits_finding",       ext_anchor_no_href_emits_finding)
  , ("ext_anchor_with_href_no_structural_finding",
        ext_anchor_with_href_no_structural_finding)
  , ("ext_alt_looks_like_filename_is_heuristic",
        ext_alt_looks_like_filename_is_heuristic)
  , ("ext_heading_skip_emits_finding",         ext_heading_skip_emits_finding)
  , ("ext_heading_consecutive_ok",             ext_heading_consecutive_ok)
  , ("ext_multiple_findings",                  ext_multiple_findings)
  , ("ext_finding_path_locates_node",          ext_finding_path_locates_node)
  , ("ext_no_main_no_finding",                 ext_no_main_no_finding)
  , ("ext_single_main_no_finding",             ext_single_main_no_finding)
  , ("ext_two_sibling_mains_fires",            ext_two_sibling_mains_fires)
  , ("ext_three_mains_one_finding",            ext_three_mains_one_finding)
  , ("pddt_alt_findings",                      pddt_alt_findings)
  , ("pbt_img_without_alt_always_reported",    pbt_img_without_alt_always_reported)
  , ("pbt_text_only_tree_no_findings",         pbt_text_only_tree_no_findings)
  , ("pbt_checkAA_total",                      pbt_checkAA_total)
  , ("ext_html_root_without_lang_fires",       ext_html_root_without_lang_fires)
  , ("ext_html_root_with_lang_ok",             ext_html_root_with_lang_ok)
  , ("ext_html_root_empty_lang_fires",         ext_html_root_empty_lang_fires)
  , ("ext_document_lang_only_at_root",         ext_document_lang_only_at_root)
  , ("ext_iframe_without_title_fires",         ext_iframe_without_title_fires)
  , ("ext_iframe_with_title_ok",               ext_iframe_with_title_ok)
  , ("ext_label_with_for_ok",                  ext_label_with_for_ok)
  , ("ext_label_with_implicit_control_ok",     ext_label_with_implicit_control_ok)
  , ("ext_label_without_for_or_control_fires", ext_label_without_for_or_control_fires)
  , ("ext_fieldset_without_legend_fires",      ext_fieldset_without_legend_fires)
  , ("ext_fieldset_with_legend_ok",            ext_fieldset_with_legend_ok)
  , ("ext_link_without_text_fires",            ext_link_without_text_fires)
  , ("ext_link_with_text_ok",                  ext_link_with_text_ok)
  , ("ext_link_with_aria_label_ok",            ext_link_with_aria_label_ok)
  , ("ext_anchor_without_href_skips_link_name", ext_anchor_without_href_skips_link_name)
  , ("ext_button_without_text_fires",          ext_button_without_text_fires)
  , ("ext_button_with_text_ok",                ext_button_with_text_ok)
  , ("ext_duplicate_id_fires",                 ext_duplicate_id_fires)
  , ("ext_unique_ids_ok",                      ext_unique_ids_ok)
  , ("ext_triplicate_id_fires_twice",          ext_triplicate_id_fires_twice)
  ]
