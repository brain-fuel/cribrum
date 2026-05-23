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
||| diverges. Observed by running on arbitrary valid trees.
export
pbt_checkAA_total : Property
pbt_checkAA_total = property $ do
  -- generate a tree of safe known-tag elements so IsValidHtml succeeds
  let knownGen = element $ the (Vect _ String)
        ["p", "div", "span", "section", "ul", "li", "h1", "h2"]
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
  , ("pddt_alt_findings",                      pddt_alt_findings)
  , ("pbt_img_without_alt_always_reported",    pbt_img_without_alt_always_reported)
  , ("pbt_text_only_tree_no_findings",         pbt_text_only_tree_no_findings)
  , ("pbt_checkAA_total",                      pbt_checkAA_total)
  ]
