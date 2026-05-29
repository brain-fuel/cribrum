module Test.Cribrum.Node

import Hedgehog
import Cribrum.Node
import Test.Cribrum.Gen

%default total

||| Single-test runner: drop sample-count from 100 -> 1 for example tests so
||| failures aren't masked by re-runs and the report stays readable.
oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs (example tests): anchor cases for each shape of HExpr.
--------------------------------------------------------------------------------

export
ext_size_text_is_one : Property
ext_size_text_is_one = oneShot $ size (Text "hi") === 1

export
ext_size_empty_element_is_one : Property
ext_size_empty_element_is_one = oneShot $ size (Element "p" [] []) === 1

export
ext_size_nested_counts_descendants : Property
ext_size_nested_counts_descendants = oneShot $
  let tree = Element "section" []
               [ Element "h1" [] [Text "title"]
               , Element "p"  [] [Text "body"]
               ]
  -- section + h1 + "title" + p + "body" = 5
   in size tree === 5

export
ext_depth_text_is_one : Property
ext_depth_text_is_one = oneShot $ depth (Text "x") === 1

export
ext_depth_nested : Property
ext_depth_nested = oneShot $
  depth (Element "a" [] [Element "b" [] [Text "c"]]) === 3

export
ext_walk_preorder : Property
ext_walk_preorder = oneShot $
  let tree = Element "ul" []
               [ Element "li" [] [Text "x"]
               , Element "li" [] [Text "y"]
               ]
      tags = map describe (walk tree)
   in tags === ["E:ul", "E:li", "T:x", "E:li", "T:y"]
  where
    describe : HExpr -> String
    describe (Element t _ _) = "E:" ++ t
    describe (Text s)        = "T:" ++ s
    describe (Comment s)     = "C:" ++ s
    describe (Raw s)         = "R:" ++ s

export
ext_descendants_excludes_root : Property
ext_descendants_excludes_root = oneShot $
  let tree = Element "p" [] [Text "a", Text "b"]
   in length (descendants tree) === 2

export
ext_eq_distinguishes_attrs : Property
ext_eq_distinguishes_attrs = oneShot $
  (Element "p" [MkHAttr "id" (Str "a")] [])
    /== (Element "p" [MkHAttr "id" (Str "b")] [])

export
ext_eq_handler_value : Property
ext_eq_handler_value = oneShot $
  (MkHAttr "on" (Handler "click" "h1"))
    /== (MkHAttr "on" (Handler "click" "h2"))

--------------------------------------------------------------------------------
-- PDDTs (parameterized, data-driven tests): tables.
--------------------------------------------------------------------------------

||| size cases: (expected, tree).
sizeCases : List (Nat, HExpr)
sizeCases =
  [ (1, Text "")
  , (1, Text "abc")
  , (1, Comment "x")
  , (1, Element "br" [] [])
  , (2, Element "p" [] [Text "hi"])
  , (3, Element "p" [] [Text "a", Text "b"])
  , (5, Element "section" []
          [ Element "h1" [] [Text "t"]
          , Element "p"  [] [Text "b"]
          ])
  , (4, Element "a" [] [Element "b" [] [Element "c" [] [Text "d"]]])
  ]

export
pddt_size_table : Property
pddt_size_table = withTests 1 . property $ do
  for_ sizeCases $ \(expected, tree) =>
    size tree === expected

depthCases : List (Nat, HExpr)
depthCases =
  [ (1, Text "")
  , (1, Comment "x")
  , (1, Element "br" [] [])
  , (2, Element "p" [] [Text "hi"])
  , (3, Element "a" [] [Element "b" [] [Text "c"]])
  , (4, Element "a" [] [Element "b" [] [Element "c" [] [Text "d"]]])
  -- mixed-depth siblings take the max (ul > li > span > text = 4)
  , (4, Element "ul" []
          [ Element "li" [] [Text "x"]
          , Element "li" [] [Element "span" [] [Text "y"]]
          ])
  ]

export
pddt_depth_table : Property
pddt_depth_table = withTests 1 . property $ do
  for_ depthCases $ \(expected, tree) =>
    depth tree === expected

--------------------------------------------------------------------------------
-- PBTs (property-based tests): invariants over generated HExpr.
--------------------------------------------------------------------------------

||| size >= 1 for every HExpr (the root always counts).
export
pbt_size_at_least_one : Property
pbt_size_at_least_one = property $ do
  h <- forAll hexpr
  diff (size h) (>=) 1

||| depth >= 1 always (the root level is depth 1).
export
pbt_depth_at_least_one : Property
pbt_depth_at_least_one = property $ do
  h <- forAll hexpr
  diff (depth h) (>=) 1

||| depth <= size: a degenerate chain still satisfies it.
export
pbt_depth_le_size : Property
pbt_depth_le_size = property $ do
  h <- forAll hexpr
  diff (depth h) (<=) (size h)

||| walk length == size: traversal visits every node exactly once.
export
pbt_walk_length_eq_size : Property
pbt_walk_length_eq_size = property $ do
  h <- forAll hexpr
  length (walk h) === size h

||| walk starts at the root.
export
pbt_walk_head_is_root : Property
pbt_walk_head_is_root = property $ do
  h <- forAll hexpr
  case walk h of
    []      => failWith Nothing "walk produced empty list"
    (x::_)  => x === h

||| Reflexivity of equality.
export
pbt_eq_reflexive : Property
pbt_eq_reflexive = property $ do
  h <- forAll hexpr
  assert (h == h)

||| descendants are exactly walk minus the root.
export
pbt_descendants_is_walk_tail : Property
pbt_descendants_is_walk_tail = property $ do
  h <- forAll hexpr
  case walk h of
    []      => failWith Nothing "walk produced empty list"
    (_::xs) => descendants h === xs

export
group : Group
group = MkGroup "Cribrum.Node"
  [ ("ext_size_text_is_one",            ext_size_text_is_one)
  , ("ext_size_empty_element_is_one",   ext_size_empty_element_is_one)
  , ("ext_size_nested_counts_descendants", ext_size_nested_counts_descendants)
  , ("ext_depth_text_is_one",           ext_depth_text_is_one)
  , ("ext_depth_nested",                ext_depth_nested)
  , ("ext_walk_preorder",               ext_walk_preorder)
  , ("ext_descendants_excludes_root",   ext_descendants_excludes_root)
  , ("ext_eq_distinguishes_attrs",      ext_eq_distinguishes_attrs)
  , ("ext_eq_handler_value",            ext_eq_handler_value)
  , ("pddt_size_table",                 pddt_size_table)
  , ("pddt_depth_table",                pddt_depth_table)
  , ("pbt_size_at_least_one",           pbt_size_at_least_one)
  , ("pbt_depth_at_least_one",          pbt_depth_at_least_one)
  , ("pbt_depth_le_size",               pbt_depth_le_size)
  , ("pbt_walk_length_eq_size",         pbt_walk_length_eq_size)
  , ("pbt_walk_head_is_root",           pbt_walk_head_is_root)
  , ("pbt_eq_reflexive",                pbt_eq_reflexive)
  , ("pbt_descendants_is_walk_tail",    pbt_descendants_is_walk_tail)
  ]
