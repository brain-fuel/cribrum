module Test.Cribrum.Html.Valid

import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.Html.Valid
import Test.Cribrum.Gen

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_text_is_valid : Property
ext_text_is_valid = oneShot $
  isValidHtml (Text "hi") === True

export
ext_comment_is_valid : Property
ext_comment_is_valid = oneShot $
  isValidHtml (Comment "x") === True

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
  -- Just observing the boolean is enough; if `decideHtml` were partial the
  -- runtime would diverge or crash. The assertion is trivial.
  let b = isValidHtml h
  diff b (\_,_ => True) b

||| Generate a tree built only from `knownTags` — the result must validate.
||| This pins the soundness direction: "constructed from valid pieces => Yes".
knownTagsGen : Gen String
knownTagsGen = element $ the (Vect _ String)
  [ "p", "div", "span", "section", "article", "ul", "li"
  , "a", "img", "h1", "h2", "h3", "nav", "aside"
  , "figure", "figcaption", "main", "header", "footer"
  ]

knownTreeAt : (depthBudget : Nat) -> Gen HExpr
knownTreeAt 0 = Text <$> string (linear 0 8) ascii
knownTreeAt (S k) = choice $ the (Vect _ (Gen HExpr))
  [ Text <$> string (linear 0 8) ascii
  , Comment <$> string (linear 0 8) ascii
  , [| Element knownTagsGen (pure []) (list (linear 0 3) (knownTreeAt k)) |]
  ]

export
pbt_known_trees_validate : Property
pbt_known_trees_validate = property $ do
  h <- forAll (knownTreeAt 3)
  isValidHtml h === True

||| Generate a tree, replace its root tag with a non-`knownTags` string. The
||| result must NOT validate. Pins the completeness direction at the root.
unknownTagsGen : Gen String
unknownTagsGen = element $ the (Vect _ String)
  [ "bogus", "marquee", "blink", "wat", "frobnicate", "xx" ]

export
pbt_unknown_root_tag_invalidates : Property
pbt_unknown_root_tag_invalidates = property $ do
  cs  <- forAll (list (linear 0 3) (knownTreeAt 2))
  tag <- forAll unknownTagsGen
  isValidHtml (Element tag [] cs) === False

export
group : Group
group = MkGroup "Cribrum.Html.Valid"
  [ ("ext_text_is_valid",                 ext_text_is_valid)
  , ("ext_comment_is_valid",              ext_comment_is_valid)
  , ("ext_known_empty_element_valid",     ext_known_empty_element_valid)
  , ("ext_paragraph_with_text_valid",     ext_paragraph_with_text_valid)
  , ("ext_unknown_tag_invalid",           ext_unknown_tag_invalid)
  , ("ext_unknown_child_invalidates_parent",
        ext_unknown_child_invalidates_parent)
  , ("ext_deeply_nested_known_valid",     ext_deeply_nested_known_valid)
  , ("ext_deeply_nested_with_bogus_invalid",
        ext_deeply_nested_with_bogus_invalid)
  , ("ext_dec_produces_proof",            ext_dec_produces_proof)
  , ("ext_dec_produces_refutation",       ext_dec_produces_refutation)
  , ("pddt_validity_table",               pddt_validity_table)
  , ("pbt_dec_total",                     pbt_dec_total)
  , ("pbt_known_trees_validate",          pbt_known_trees_validate)
  , ("pbt_unknown_root_tag_invalidates",  pbt_unknown_root_tag_invalidates)
  ]
