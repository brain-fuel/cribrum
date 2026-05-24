module Test.Cribrum.Elaborate

import Data.Vect
import Hedgehog
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser
import Cribrum.Html.Valid
import Cribrum.Elaborate

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

para : String -> Block
para s = Paragraph emptyAttrs [InlText s]

heading : Nat -> String -> Block
heading n s = Heading emptyAttrs n [InlText s]

doc : List Block -> Doc
doc = MkDoc

export
ext_empty_doc_elaborates_to_empty_main : Property
ext_empty_doc_elaborates_to_empty_main = oneShot $
  elaborateDoc (doc []) === Element "main" [] []

export
ext_paragraph_becomes_p : Property
ext_paragraph_becomes_p = oneShot $
  elaborateDoc (doc [para "hi"])
    === Element "main" [] [Element "p" [] [Text "hi"]]

export
ext_heading_levels_map_to_h_tags : Property
ext_heading_levels_map_to_h_tags = oneShot $
  elaborateDoc (doc [heading 3 "T"])
    === Element "main" [] [Element "h3" [] [Text "T"]]

export
ext_thematic_becomes_hr : Property
ext_thematic_becomes_hr = oneShot $
  elaborateDoc (doc [ThematicBreak emptyAttrs])
    === Element "main" [] [Element "hr" [] []]

export
ext_softbreak_becomes_space : Property
ext_softbreak_becomes_space = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs
           [InlText "a", InlSoftBreak, InlText "b"]])
    === Element "main" []
          [Element "p" [] [Text "a", Text " ", Text "b"]]

export
ext_hardbreak_becomes_br : Property
ext_hardbreak_becomes_br = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlText "a", InlHardBreak, InlText "b"]])
    === Element "main" []
          [Element "p" [] [Text "a", Element "br" [] [], Text "b"]]

export
ext_emphasis_becomes_em : Property
ext_emphasis_becomes_em = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlEmph [InlText "x"]]])
    === Element "main" []
          [Element "p" [] [Element "em" [] [Text "x"]]]

export
ext_strong_becomes_strong : Property
ext_strong_becomes_strong = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlStrong [InlText "x"]]])
    === Element "main" []
          [Element "p" [] [Element "strong" [] [Text "x"]]]

||| Strict elaboration: the produced HExpr passes IsValidHtml; the
||| Either-Right carries the witness.
export
ext_strict_elaboration_carries_proof : Property
ext_strict_elaboration_carries_proof = oneShot $
  case elaborate (doc [para "hi", heading 2 "T"]) of
    Right _ => success
    Left  e => failWith Nothing ("expected Right; got " ++ show e)

||| Round trip: parsed input -> elaborated HExpr is structurally valid HTML.
export
ext_round_trip_parse_then_elaborate : Property
ext_round_trip_parse_then_elaborate = oneShot $
  case parseDoc "# Title\n\nA paragraph." of
    Right d => case elaborate d of
      Right _ => success
      Left  e => failWith Nothing ("elaborate failed: " ++ show e)
    Left e  => failWith Nothing ("parse failed: " ++ show e)

||| Strict elaboration of a skipped heading sequence (h1 -> h3) is rejected
||| with the `heading-no-skip` rule id surfaced through `StructuralAaFailure`.
||| Pins the Phase-4 codomain wiring: a structurally-AA failure short-circuits
||| through `decStructuralAA` and is reported with its catalog id.
export
ext_strict_elaboration_rejects_skipped_headings : Property
ext_strict_elaboration_rejects_skipped_headings = oneShot $
  case elaborate (doc [heading 1 "A", heading 3 "C"]) of
    Right _                            =>
      failWith Nothing "expected StructuralAaFailure heading-no-skip"
    Left (StructuralAaFailure "heading-no-skip") => success
    Left e =>
      failWith Nothing ("wrong error variant: " ++ show e)

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

||| Heading level -> tag name (1..6).
headingTagCases : List (Nat, String)
headingTagCases =
  [ (1, "h1"), (2, "h2"), (3, "h3"), (4, "h4"), (5, "h5"), (6, "h6") ]

export
pddt_heading_tags : Property
pddt_heading_tags = withTests 1 . property $ do
  for_ headingTagCases $ \(lvl, tag) =>
    elaborateBlock (heading lvl "T")
      === Element tag [] [Text "T"]

||| Inline-construct -> tag mapping; pin the catalog.
inlineMappingCases : List (Inline, HExpr)
inlineMappingCases =
  [ (InlText "x",                       Text "x")
  , (InlSoftBreak,                      Text " ")
  , (InlHardBreak,                      Element "br" [] [])
  , (InlEmph    [InlText "x"],          Element "em"     [] [Text "x"])
  , (InlStrong  [InlText "x"],          Element "strong" [] [Text "x"])
  , (InlHighlight [InlText "x"],        Element "mark"   [] [Text "x"])
  , (InlSuper   [InlText "x"],          Element "sup"    [] [Text "x"])
  , (InlSub     [InlText "x"],          Element "sub"    [] [Text "x"])
  , (InlInsert  [InlText "x"],          Element "ins"    [] [Text "x"])
  , (InlDelete  [InlText "x"],          Element "del"    [] [Text "x"])
  , (InlVerbatim emptyAttrs "code",     Element "code"   [] [Text "code"])
  , (InlSpan emptyAttrs [InlText "x"],  Element "span"   [] [Text "x"])
  , (InlSmart EmDash,                   Text "\x2014")
  , (InlSmart Ellipsis,                 Text "\x2026")
  ]

export
pddt_inline_mapping : Property
pddt_inline_mapping = withTests 1 . property $ do
  for_ inlineMappingCases $ \(input, expected) =>
    elaborateInline input === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

genHeadingLevel : Gen Nat
genHeadingLevel = nat $ constant 1 6

genSimpleInlines : Gen (List Inline)
genSimpleInlines = list (linear 0 4) $
  choice $ the (Vect _ (Gen Inline))
    [ InlText <$> string (linear 0 8) ascii
    , pure InlSoftBreak
    , pure InlHardBreak
    , (\s => InlEmph   [InlText s]) <$> string (linear 0 4) ascii
    , (\s => InlStrong [InlText s]) <$> string (linear 0 4) ascii
    ]

||| Clamp any heading sequence so it satisfies the Phase-4 heading-no-skip
||| rule (first heading any level; each subsequent heading <= prev + 1). The
||| free generator otherwise emits skips like `[h1, h3]` which strict
||| elaboration now (rightly) rejects.
normalizeHeadings : List Block -> List Block
normalizeHeadings = go Nothing
  where
    go : Maybe Nat -> List Block -> List Block
    go _    []        = []
    go prev (Heading attrs lvl is :: rest) =
      let lvl' = case prev of
            Nothing => lvl
            Just p  => if lvl > S p then S p else lvl
       in Heading attrs lvl' is :: go (Just lvl') rest
    go prev (b :: rest) = b :: go prev rest

genSimpleBlocks : Gen (List Block)
genSimpleBlocks = normalizeHeadings <$> (list (linear 0 4) $
  choice $ the (Vect _ (Gen Block))
    [ Paragraph emptyAttrs <$> genSimpleInlines
    , [| Heading (pure emptyAttrs) genHeadingLevel genSimpleInlines |]
    , pure (ThematicBreak emptyAttrs)
    ])

genSimpleDoc : Gen Doc
genSimpleDoc = MkDoc <$> genSimpleBlocks

||| Strict-mode contract: every Doc produced by `genSimpleDoc` elaborates
||| without error, i.e. lands in the proof-carrying codomain.
export
pbt_strict_elaboration_total_on_simple_docs : Property
pbt_strict_elaboration_total_on_simple_docs = property $ do
  d <- forAll genSimpleDoc
  case elaborate d of
    Right _ => success
    Left  e => failWith Nothing ("unexpected failure: " ++ show e)

||| Every elaborated document is a valid HTML tree (regardless of slice).
export
pbt_elaborated_doc_isValidHtml : Property
pbt_elaborated_doc_isValidHtml = property $ do
  d <- forAll genSimpleDoc
  isValidHtml (elaborateDoc d) === True

||| Round-trip: parse arbitrary safe ASCII text, elaborate, get valid HTML.
||| Restricted to inputs that exercise only the parser's current slice.
genParserSafeInput : Gen String
genParserSafeInput = do
  let alphaChar = element $ the (Vect _ Char)
        ['a','b','c','d','e','f','g','h','i','j','k','l','m'
        ,'n','o','p','q','r','s','t','u','v','w','x','y','z'
        ,'0','1','2','3','4','5','6','7','8','9',' ']
  -- a sequence of short non-blank lines separated by blank lines
  lines <- list (linear 0 4) $
    [| pack
         ((::) <$> element (the (Vect _ Char) ['a','b','c','x','y','z'])
               <*> list (linear 0 8) alphaChar) |]
  pure (joinWithBlank lines)
  where
    joinWithBlank : List String -> String
    joinWithBlank []        = ""
    joinWithBlank [x]       = x
    joinWithBlank (x :: xs) = x ++ "\n\n" ++ joinWithBlank xs

export
pbt_parse_elaborate_round_trip : Property
pbt_parse_elaborate_round_trip = property $ do
  s <- forAll genParserSafeInput
  case parseDoc s of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Right _ => success
      Left  e => failWith Nothing ("elaborate failed: " ++ show e)

export
group : Group
group = MkGroup "Cribrum.Elaborate"
  [ ("ext_empty_doc_elaborates_to_empty_main", ext_empty_doc_elaborates_to_empty_main)
  , ("ext_paragraph_becomes_p",                ext_paragraph_becomes_p)
  , ("ext_heading_levels_map_to_h_tags",       ext_heading_levels_map_to_h_tags)
  , ("ext_thematic_becomes_hr",                ext_thematic_becomes_hr)
  , ("ext_softbreak_becomes_space",            ext_softbreak_becomes_space)
  , ("ext_hardbreak_becomes_br",               ext_hardbreak_becomes_br)
  , ("ext_emphasis_becomes_em",                ext_emphasis_becomes_em)
  , ("ext_strong_becomes_strong",              ext_strong_becomes_strong)
  , ("ext_strict_elaboration_carries_proof",   ext_strict_elaboration_carries_proof)
  , ("ext_round_trip_parse_then_elaborate",    ext_round_trip_parse_then_elaborate)
  , ("ext_strict_elaboration_rejects_skipped_headings",
        ext_strict_elaboration_rejects_skipped_headings)
  , ("pddt_heading_tags",                      pddt_heading_tags)
  , ("pddt_inline_mapping",                    pddt_inline_mapping)
  , ("pbt_strict_elaboration_total_on_simple_docs",
        pbt_strict_elaboration_total_on_simple_docs)
  , ("pbt_elaborated_doc_isValidHtml",         pbt_elaborated_doc_isValidHtml)
  , ("pbt_parse_elaborate_round_trip",         pbt_parse_elaborate_round_trip)
  ]
