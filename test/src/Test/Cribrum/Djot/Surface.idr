module Test.Cribrum.Djot.Surface

import Data.Vect
import Hedgehog
import Cribrum.Djot.Surface

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- Generators.
--------------------------------------------------------------------------------

genAttrs : Gen Attrs
genAttrs =
  [| MkAttrs (maybe (string (linear 1 6) alphaNum))
             (list (linear 0 2) (string (linear 1 6) alphaNum))
             (list (linear 0 2)
                   [| MkPair (string (linear 1 6) alphaNum)
                             (string (linear 0 6) alphaNum) |]) |]

genSmart : Gen SmartPunct
genSmart = element $ the (Vect _ SmartPunct)
  [LDQuote, RDQuote, LSQuote, RSQuote, EnDash, EmDash, Ellipsis]

genLinkRef : Gen LinkRef
genLinkRef = choice $ the (Vect _ (Gen LinkRef))
  [ [| LinkInline (string (linear 1 12) alphaNum)
                  (maybe (string (linear 1 6) alphaNum)) |]
  , LinkReference <$> string (linear 1 8) alphaNum
  , LinkAuto      <$> string (linear 1 12) alphaNum
  ]

inlineAt : (depthBudget : Nat) -> Gen Inline
inlineAt 0 = choice $ the (Vect _ (Gen Inline))
  [ InlText <$> string (linear 0 8) ascii
  , pure InlSoftBreak
  , pure InlHardBreak
  , InlSymbol <$> string (linear 1 6) alphaNum
  , InlSmart  <$> genSmart
  ]
inlineAt (S k) = choice $ the (Vect _ (Gen Inline))
  [ InlText        <$> string (linear 0 8) ascii
  , pure InlSoftBreak
  , pure InlHardBreak
  , InlEmph        <$> list (linear 0 3) (inlineAt k)
  , InlStrong      <$> list (linear 0 3) (inlineAt k)
  , InlHighlight   <$> list (linear 0 3) (inlineAt k)
  , InlSpan        <$> genAttrs <*> list (linear 0 3) (inlineAt k)
  , [| InlLink     genAttrs genLinkRef (list (linear 0 3) (inlineAt k)) |]
  ]

mutual
  blockAt : (depthBudget : Nat) -> Gen Block
  blockAt 0 = choice $ the (Vect _ (Gen Block))
    [ Paragraph     <$> genAttrs <*> list (linear 0 4) (inlineAt 1)
    , [| Heading    genAttrs (nat $ linear 1 6)
                    (list (linear 0 4) (inlineAt 1)) |]
    , [| CodeBlock  genAttrs (string (linear 0 6) alphaNum)
                             (string (linear 0 24) ascii) |]
    , ThematicBreak <$> genAttrs
    ]
  blockAt (S k) = choice $ the (Vect _ (Gen Block))
    [ Paragraph     <$> genAttrs <*> list (linear 0 4) (inlineAt 1)
    , [| Heading    genAttrs (nat $ linear 1 6)
                    (list (linear 0 4) (inlineAt 1)) |]
    , BlockQuote    <$> genAttrs <*> list (linear 0 3) (blockAt k)
    , Div           <$> genAttrs <*> list (linear 0 3) (blockAt k)
    , [| ListBlock  genAttrs (pure UnorderedDash) (pure Nothing) (pure True)
                    (list (linear 0 3) (listItemAt k)) |]
    ]

  listItemAt : (depthBudget : Nat) -> Gen ListItem
  listItemAt k =
    [| MkLI genAttrs (pure Nothing) (pure Nothing)
            (list (linear 0 3) (blockAt k)) |]

genDoc : Gen Doc
genDoc = MkDoc <$> list (linear 0 5) (blockAt 2)

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_empty_attrs_eq : Property
ext_empty_attrs_eq =
  oneShot $ emptyAttrs === MkAttrs Nothing [] []

export
ext_blockSize_paragraph : Property
ext_blockSize_paragraph = oneShot $
  blockSize (Paragraph emptyAttrs [InlText "hi"]) === 1

export
ext_blockSize_blockquote_nested : Property
ext_blockSize_blockquote_nested = oneShot $
  let b = BlockQuote emptyAttrs
            [ Paragraph emptyAttrs [InlText "a"]
            , ThematicBreak emptyAttrs
            ]
   in blockSize b === 3

export
ext_blockSize_list_with_items : Property
ext_blockSize_list_with_items = oneShot $
  let it1 = MkLI emptyAttrs Nothing Nothing
                 [Paragraph emptyAttrs [InlText "x"]]
      it2 = MkLI emptyAttrs Nothing Nothing
                 [ Paragraph emptyAttrs [InlText "y"]
                 , Paragraph emptyAttrs [InlText "z"]
                 ]
      lb  = ListBlock emptyAttrs UnorderedDash Nothing True [it1, it2]
  -- 1 (list itself) + 1 + 2 = 4
   in blockSize lb === 4

export
ext_inlineSize_text_one : Property
ext_inlineSize_text_one = oneShot $
  inlineSize (InlText "abc") === 1

export
ext_inlineSize_nested_emph_strong : Property
ext_inlineSize_nested_emph_strong = oneShot $
  inlineSize (InlEmph [InlStrong [InlText "a", InlText "b"]]) === 4

export
ext_eq_linkref_distinguishes_constructors : Property
ext_eq_linkref_distinguishes_constructors = oneShot $
  LinkInline "u" Nothing /== LinkReference "u"

export
ext_eq_smart_distinguishes : Property
ext_eq_smart_distinguishes = oneShot $
  LDQuote /== RDQuote

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

blockSizeCases : List (Nat, Block)
blockSizeCases =
  [ (1, Paragraph emptyAttrs [])
  , (1, Heading emptyAttrs 1 [InlText "t"])
  , (1, ThematicBreak emptyAttrs)
  , (1, CodeBlock emptyAttrs "" "")
  , (1, RawBlock "html" "<br>")
  , (1, RefDef "x" "/y" Nothing emptyAttrs)
  , (1, Table emptyAttrs Nothing [])
  -- BlockQuote + 0 inner = 1; + 2 inner = 3
  , (1, BlockQuote emptyAttrs [])
  , (3, BlockQuote emptyAttrs
          [ Paragraph emptyAttrs []
          , ThematicBreak emptyAttrs
          ])
  , (1, Div emptyAttrs [])
  , (2, Div emptyAttrs [Paragraph emptyAttrs []])
  , (1, ListBlock emptyAttrs UnorderedDash Nothing True [])
  ]

export
pddt_blockSize_table : Property
pddt_blockSize_table = withTests 1 . property $ do
  for_ blockSizeCases $ \(expected, b) =>
    blockSize b === expected

inlineSizeCases : List (Nat, Inline)
inlineSizeCases =
  [ (1, InlText "")
  , (1, InlSoftBreak)
  , (1, InlHardBreak)
  , (1, InlSymbol "smile")
  , (1, InlMath False "x")
  , (1, InlSmart Ellipsis)
  , (1, InlComment "c")
  , (1, InlVerbatim emptyAttrs "code")
  , (1, InlRaw "html" "<br>")
  -- containers add 1 for self
  , (2, InlEmph [InlText "x"])
  , (3, InlStrong [InlText "a", InlText "b"])
  , (4, InlEmph [InlStrong [InlText "a", InlText "b"]])
  , (2, InlLink emptyAttrs (LinkInline "u" Nothing) [InlText "t"])
  , (2, InlImage emptyAttrs (LinkInline "u" Nothing) [InlText "alt"])
  , (2, InlSpan emptyAttrs [InlText "x"])
  ]

export
pddt_inlineSize_table : Property
pddt_inlineSize_table = withTests 1 . property $ do
  for_ inlineSizeCases $ \(expected, i) =>
    inlineSize i === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

export
pbt_blockSize_at_least_one : Property
pbt_blockSize_at_least_one = property $ do
  b <- forAll (blockAt 2)
  diff (blockSize b) (>=) 1

export
pbt_inlineSize_at_least_one : Property
pbt_inlineSize_at_least_one = property $ do
  i <- forAll (inlineAt 2)
  diff (inlineSize i) (>=) 1

||| Eq is reflexive across the whole surface AST.
export
pbt_doc_eq_reflexive : Property
pbt_doc_eq_reflexive = property $ do
  d <- forAll genDoc
  assert (d == d)

||| countBlocks distributes over concatenation: countBlocks (xs ++ ys)
||| = countBlocks xs + countBlocks ys.
export
pbt_countBlocks_homomorphism : Property
pbt_countBlocks_homomorphism = property $ do
  xs <- forAll (list (linear 0 4) (blockAt 1))
  ys <- forAll (list (linear 0 4) (blockAt 1))
  countBlocks (xs ++ ys) === countBlocks xs + countBlocks ys

||| Wrapping a block list in a Div adds exactly 1 to the count.
export
pbt_div_adds_one : Property
pbt_div_adds_one = property $ do
  bs <- forAll (list (linear 0 4) (blockAt 1))
  blockSize (Div emptyAttrs bs) === S (countBlocks bs)

||| Wrapping an inline list in InlEmph adds exactly 1 to the count.
export
pbt_emph_adds_one : Property
pbt_emph_adds_one = property $ do
  xs <- forAll (list (linear 0 4) (inlineAt 1))
  inlineSize (InlEmph xs) === S (sum (map inlineSize xs))

export
group : Group
group = MkGroup "Cribrum.Djot.Surface"
  [ ("ext_empty_attrs_eq",                  ext_empty_attrs_eq)
  , ("ext_blockSize_paragraph",             ext_blockSize_paragraph)
  , ("ext_blockSize_blockquote_nested",     ext_blockSize_blockquote_nested)
  , ("ext_blockSize_list_with_items",       ext_blockSize_list_with_items)
  , ("ext_inlineSize_text_one",             ext_inlineSize_text_one)
  , ("ext_inlineSize_nested_emph_strong",   ext_inlineSize_nested_emph_strong)
  , ("ext_eq_linkref_distinguishes_constructors",
        ext_eq_linkref_distinguishes_constructors)
  , ("ext_eq_smart_distinguishes",          ext_eq_smart_distinguishes)
  , ("pddt_blockSize_table",                pddt_blockSize_table)
  , ("pddt_inlineSize_table",               pddt_inlineSize_table)
  , ("pbt_blockSize_at_least_one",          pbt_blockSize_at_least_one)
  , ("pbt_inlineSize_at_least_one",         pbt_inlineSize_at_least_one)
  , ("pbt_doc_eq_reflexive",                pbt_doc_eq_reflexive)
  , ("pbt_countBlocks_homomorphism",        pbt_countBlocks_homomorphism)
  , ("pbt_div_adds_one",                    pbt_div_adds_one)
  , ("pbt_emph_adds_one",                   pbt_emph_adds_one)
  ]
