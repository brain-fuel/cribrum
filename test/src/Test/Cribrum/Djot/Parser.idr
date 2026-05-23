module Test.Cribrum.Djot.Parser

import Data.String
import Data.Vect
import Hedgehog
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

-- Convenience constructors keeping the EXT bodies readable.

para : String -> Block
para s = Paragraph emptyAttrs [InlText s]

paraMulti : List Inline -> Block
paraMulti is = Paragraph emptyAttrs is

heading : (level : Nat) -> String -> Block
heading n s = Heading emptyAttrs n [InlText s]

doc : List Block -> Doc
doc = MkDoc

ok : Doc -> Either ParseError Doc
ok = Right

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_empty_input_empty_doc : Property
ext_empty_input_empty_doc = oneShot $
  parseDoc "" === ok (doc [])

export
ext_blank_only_empty_doc : Property
ext_blank_only_empty_doc = oneShot $
  parseDoc "   \n\t\n" === ok (doc [])

export
ext_single_line_paragraph : Property
ext_single_line_paragraph = oneShot $
  parseDoc "hello" === ok (doc [para "hello"])

export
ext_h1 : Property
ext_h1 = oneShot $
  parseDoc "# Title" === ok (doc [heading 1 "Title"])

export
ext_h2 : Property
ext_h2 = oneShot $
  parseDoc "## Sub" === ok (doc [heading 2 "Sub"])

export
ext_h6 : Property
ext_h6 = oneShot $
  parseDoc "###### deep" === ok (doc [heading 6 "deep"])

||| `#######` (7 hashes) is *not* a Djot heading — falls through to paragraph.
export
ext_seven_hashes_is_paragraph : Property
ext_seven_hashes_is_paragraph = oneShot $
  parseDoc "####### nope" === ok (doc [para "####### nope"])

||| `#title` (no space) is *not* a heading.
export
ext_hash_no_space_is_paragraph : Property
ext_hash_no_space_is_paragraph = oneShot $
  parseDoc "#title" === ok (doc [para "#title"])

export
ext_multiline_paragraph_softbreak : Property
ext_multiline_paragraph_softbreak = oneShot $
  parseDoc "line1\nline2"
    === ok (doc [paraMulti [InlText "line1", InlSoftBreak, InlText "line2"]])

export
ext_two_paragraphs : Property
ext_two_paragraphs = oneShot $
  parseDoc "p1\n\np2" === ok (doc [para "p1", para "p2"])

export
ext_heading_then_paragraph : Property
ext_heading_then_paragraph = oneShot $
  parseDoc "# H\n\nbody"
    === ok (doc [heading 1 "H", para "body"])

||| A leading blank line is just a separator; one paragraph still results.
export
ext_leading_blank_ignored : Property
ext_leading_blank_ignored = oneShot $
  parseDoc "\nhi" === ok (doc [para "hi"])

||| Trailing blank lines do not create empty blocks.
export
ext_trailing_blank_ignored : Property
ext_trailing_blank_ignored = oneShot $
  parseDoc "hi\n\n\n" === ok (doc [para "hi"])

||| Multi-line block whose *first* line looks like a heading is currently a
||| paragraph (multi-line headings will arrive in a later slice with a setext-
||| like or `>` annotation). Pin the current behaviour.
export
ext_heading_marker_with_following_line_is_paragraph : Property
ext_heading_marker_with_following_line_is_paragraph = oneShot $
  parseDoc "# H\nmore"
    === ok (doc [paraMulti [InlText "# H", InlSoftBreak, InlText "more"]])

||| Line starting with a space (no '#') is a paragraph, NOT a heading-of-level-0.
||| (Mutant-kill: bounds 6->5/7 are tested; this pins the lower bound at 1.)
export
ext_space_leading_line_is_paragraph : Property
ext_space_leading_line_is_paragraph = oneShot $
  parseDoc " hello" === ok (doc [para " hello"])

||| `# ` (marker + space, empty body) is a heading with EMPTY inline content,
||| not a heading carrying `InlText ""`. (Mutant-kill on parseInlineLine "".)
export
ext_heading_empty_body : Property
ext_heading_empty_body = oneShot $
  parseDoc "# " === ok (doc [Heading emptyAttrs 1 []])

--- Thematic breaks -----------------------------------------------------------

export
ext_thematic_dashes : Property
ext_thematic_dashes = oneShot $
  parseDoc "---" === ok (doc [ThematicBreak emptyAttrs])

export
ext_thematic_stars : Property
ext_thematic_stars = oneShot $
  parseDoc "***" === ok (doc [ThematicBreak emptyAttrs])

export
ext_thematic_many_dashes : Property
ext_thematic_many_dashes = oneShot $
  parseDoc "----------" === ok (doc [ThematicBreak emptyAttrs])

export
ext_thematic_with_surrounding_whitespace : Property
ext_thematic_with_surrounding_whitespace = oneShot $
  parseDoc "   ---   " === ok (doc [ThematicBreak emptyAttrs])

||| Two dashes is NOT a thematic break — falls through to paragraph.
export
ext_two_dashes_is_paragraph : Property
ext_two_dashes_is_paragraph = oneShot $
  parseDoc "--" === ok (doc [para "--"])

||| Mixed `-`/`*` is NOT a thematic break.
export
ext_mixed_dashes_stars_is_paragraph : Property
ext_mixed_dashes_stars_is_paragraph = oneShot $
  parseDoc "-*-" === ok (doc [para "-*-"])

||| Thematic break separates paragraphs even without a blank line on either
||| side... but per current grouping logic, a thematic break only resolves
||| when it stands alone in its group. Pin the surrounding-blank-line case.
export
ext_para_then_break_then_para : Property
ext_para_then_break_then_para = oneShot $
  parseDoc "before\n\n---\n\nafter"
    === ok (doc [para "before", ThematicBreak emptyAttrs, para "after"])

||| A `---` line GLUED to a paragraph (no surrounding blank line) is
||| currently absorbed into the paragraph. This is a known limitation of the
||| single-pass slice; a later iteration will treat a thematic break as a
||| paragraph terminator. Pin the current behaviour so the regression is
||| explicit.
export
ext_dashes_glued_to_paragraph_is_paragraph : Property
ext_dashes_glued_to_paragraph_is_paragraph = oneShot $
  parseDoc "hi\n---"
    === ok (doc [paraMulti [InlText "hi", InlSoftBreak, InlText "---"]])

||| Symmetric case: `---` as the FIRST line of a multi-line group is also
||| absorbed (NOT a thematic break that drops the rest of the group).
||| Pins the `isNil ls && ...` guard against mutants that drop the `isNil ls`.
export
ext_dashes_first_in_multi_line_group_is_paragraph : Property
ext_dashes_first_in_multi_line_group_is_paragraph = oneShot $
  parseDoc "---\nbody"
    === ok (doc [paraMulti [InlText "---", InlSoftBreak, InlText "body"]])

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

||| Heading levels 1..6: `n * '#' ++ " " ++ "T"` parses as Heading n "T".
headingCases : List (Nat, String)
headingCases =
  [ (1, "#")
  , (2, "##")
  , (3, "###")
  , (4, "####")
  , (5, "#####")
  , (6, "######")
  ]

export
pddt_heading_levels : Property
pddt_heading_levels = withTests 1 . property $ do
  for_ headingCases $ \(lvl, marker) =>
    parseDoc (marker ++ " T") === ok (doc [heading lvl "T"])

||| 0 or 7+ `#` is never a heading.
nonHeadingCases : List String
nonHeadingCases =
  [ "T"             -- 0
  , "####### x"     -- 7
  , "######## x"    -- 8
  , "############ x"
  ]

export
pddt_non_heading_levels : Property
pddt_non_heading_levels = withTests 1 . property $ do
  for_ nonHeadingCases $ \s =>
    parseDoc s === ok (doc [para s])

||| Various blank-only inputs all elaborate to the empty doc.
blankInputs : List String
blankInputs = ["", " ", "\n", "  \n\t\n  ", "\n\n\n"]

export
pddt_blank_inputs : Property
pddt_blank_inputs = withTests 1 . property $ do
  for_ blankInputs $ \s =>
    parseDoc s === ok (doc [])

||| All thematic-break forms parse to a single ThematicBreak block.
thematicCases : List String
thematicCases =
  [ "---"
  , "----"
  , "-----"
  , "***"
  , "****"
  , "  ---  "
  , "\t---\t"
  , "---------------"
  ]

export
pddt_thematic_breaks : Property
pddt_thematic_breaks = withTests 1 . property $ do
  for_ thematicCases $ \s =>
    parseDoc s === ok (doc [ThematicBreak emptyAttrs])

||| Variants that look thematic-ish but are NOT.
nonThematicCases : List String
nonThematicCases =
  [ "--"        -- only 2
  , "**"
  , "- - -"     -- whitespace-separated form — later slice
  , "-*-"       -- mixed
  , "-=-"       -- non-marker chars
  , "===---"    -- garbage prefix
  ]

export
pddt_non_thematic : Property
pddt_non_thematic = withTests 1 . property $ do
  for_ nonThematicCases $ \s =>
    parseDoc s === ok (doc [para s])

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

||| `parseDoc` is total and never returns Left on the current slice.
export
pbt_parser_total : Property
pbt_parser_total = property $ do
  s <- forAll (string (linear 0 80) ascii)
  case parseDoc s of
    Right _ => success
    Left  e => failWith Nothing ("unexpected error: " ++ show e)

||| Single-line, non-empty, no leading '#': always yields one paragraph block
||| whose inline content is exactly `[InlText s]`.
safeWordChar : Gen Char
safeWordChar = element $ the (Vect _ Char)
  ['a','b','c','d','e','f','g','h','i','j','k','l','m'
  ,'n','o','p','q','r','s','t','u','v','w','x','y','z'
  ,'0','1','2','3','4','5','6','7','8','9',' ']

export
pbt_safe_single_line_is_paragraph : Property
pbt_safe_single_line_is_paragraph = property $ do
  -- ensure first char is a letter so it can't start with '#'
  c   <- forAll $ element $ the (Vect _ Char)
           ['a','b','c','x','y','z','A','M','Z']
  rest <- forAll (string (linear 0 24) safeWordChar)
  let s = pack (c :: unpack rest)
  parseDoc s === ok (doc [para s])

||| Block count == number of non-blank groups in the input. Use chunks built
||| from non-space alphanumerics so each generated chunk is guaranteed to be a
||| non-blank line and thus exactly one paragraph block.
export
pbt_block_count_eq_group_count : Property
pbt_block_count_eq_group_count = property $ do
  let chunkChar = element $ the (Vect _ Char)
        ['a','b','c','d','e','f','g','h','i','j','k','l','m'
        ,'n','o','p','q','r','s','t','u','v','w','x','y','z'
        ,'0','1','2','3','4','5','6','7','8','9']
  chunks <- forAll $ list (linear 0 4)
              [| pack (list (linear 1 6) chunkChar) |]
  let src = unwords' chunks
  case parseDoc src of
    Right (MkDoc bs) => length bs === length chunks
    Left e           => failWith Nothing (show e)
  where
    unwords' : List String -> String
    unwords' = go
      where
        go : List String -> String
        go []        = ""
        go [x]       = x
        go (x :: xs) = x ++ "\n\n" ++ go xs

export
group : Group
group = MkGroup "Cribrum.Djot.Parser"
  [ ("ext_empty_input_empty_doc",                ext_empty_input_empty_doc)
  , ("ext_blank_only_empty_doc",                 ext_blank_only_empty_doc)
  , ("ext_single_line_paragraph",                ext_single_line_paragraph)
  , ("ext_h1",                                   ext_h1)
  , ("ext_h2",                                   ext_h2)
  , ("ext_h6",                                   ext_h6)
  , ("ext_seven_hashes_is_paragraph",            ext_seven_hashes_is_paragraph)
  , ("ext_hash_no_space_is_paragraph",           ext_hash_no_space_is_paragraph)
  , ("ext_multiline_paragraph_softbreak",        ext_multiline_paragraph_softbreak)
  , ("ext_two_paragraphs",                       ext_two_paragraphs)
  , ("ext_heading_then_paragraph",               ext_heading_then_paragraph)
  , ("ext_leading_blank_ignored",                ext_leading_blank_ignored)
  , ("ext_trailing_blank_ignored",               ext_trailing_blank_ignored)
  , ("ext_heading_marker_with_following_line_is_paragraph",
        ext_heading_marker_with_following_line_is_paragraph)
  , ("ext_space_leading_line_is_paragraph",      ext_space_leading_line_is_paragraph)
  , ("ext_heading_empty_body",                   ext_heading_empty_body)
  , ("ext_thematic_dashes",                      ext_thematic_dashes)
  , ("ext_thematic_stars",                       ext_thematic_stars)
  , ("ext_thematic_many_dashes",                 ext_thematic_many_dashes)
  , ("ext_thematic_with_surrounding_whitespace", ext_thematic_with_surrounding_whitespace)
  , ("ext_two_dashes_is_paragraph",              ext_two_dashes_is_paragraph)
  , ("ext_mixed_dashes_stars_is_paragraph",      ext_mixed_dashes_stars_is_paragraph)
  , ("ext_para_then_break_then_para",            ext_para_then_break_then_para)
  , ("ext_dashes_glued_to_paragraph_is_paragraph",
        ext_dashes_glued_to_paragraph_is_paragraph)
  , ("ext_dashes_first_in_multi_line_group_is_paragraph",
        ext_dashes_first_in_multi_line_group_is_paragraph)
  , ("pddt_thematic_breaks",                     pddt_thematic_breaks)
  , ("pddt_non_thematic",                        pddt_non_thematic)
  , ("pddt_heading_levels",                      pddt_heading_levels)
  , ("pddt_non_heading_levels",                  pddt_non_heading_levels)
  , ("pddt_blank_inputs",                        pddt_blank_inputs)
  , ("pbt_parser_total",                         pbt_parser_total)
  , ("pbt_safe_single_line_is_paragraph",        pbt_safe_single_line_is_paragraph)
  , ("pbt_block_count_eq_group_count",           pbt_block_count_eq_group_count)
  ]
