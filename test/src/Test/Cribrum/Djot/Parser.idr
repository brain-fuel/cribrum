module Test.Cribrum.Djot.Parser

import Data.List
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

||| Two dashes is NOT a thematic break — falls through to paragraph,
||| where the smart-punctuation pass converts it to an en-dash.
export
ext_two_dashes_is_paragraph : Property
ext_two_dashes_is_paragraph = oneShot $
  parseDoc "--" === ok (doc [paraMulti [InlSmart EnDash]])

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
||| absorbed into the paragraph, where the smart-punctuation pass then
||| converts it into an em-dash. This is a known limitation of the
||| single-pass block slice; a later iteration will treat a thematic
||| break as a paragraph terminator. Pin the current behaviour so the
||| regression is explicit.
export
ext_dashes_glued_to_paragraph_is_paragraph : Property
ext_dashes_glued_to_paragraph_is_paragraph = oneShot $
  parseDoc "hi\n---"
    === ok (doc [paraMulti [InlText "hi", InlSoftBreak, InlSmart EmDash]])

||| Symmetric case: `---` as the FIRST line of a multi-line group is
||| also absorbed into the paragraph (NOT a thematic break that drops
||| the rest of the group) and smart-puncts to an em-dash. Pins the
||| `isNil ls && ...` guard against mutants that drop the `isNil ls`.
export
ext_dashes_first_in_multi_line_group_is_paragraph : Property
ext_dashes_first_in_multi_line_group_is_paragraph = oneShot $
  parseDoc "---\nbody"
    === ok (doc [paraMulti [InlSmart EmDash, InlSoftBreak, InlText "body"]])

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

||| Variants that look thematic-ish but are NOT a block-level
||| ThematicBreak. Some now smart-punct inside the paragraph (e.g.
||| `--` becomes en-dash, `---` em-dash) but the block-level claim
||| holds: none of these inputs produces a ThematicBreak.
nonThematicCases : List (String, Block)
nonThematicCases =
  [ ("--",      paraMulti [InlSmart EnDash])
  , ("**",      para "**")
  , ("-*-",     para "-*-")
  , ("-=-",     para "-=-")
  , ("===---",  paraMulti [InlText "===", InlSmart EmDash])
  ]

export
pddt_non_thematic : Property
pddt_non_thematic = withTests 1 . property $ do
  for_ nonThematicCases $ \(s, expected) =>
    parseDoc s === ok (doc [expected])

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

--- Code blocks --------------------------------------------------------------

code : String -> String -> Block
code info body = CodeBlock emptyAttrs info body

export
ext_code_block_empty : Property
ext_code_block_empty = oneShot $
  parseDoc "```\n```" === ok (doc [code "" ""])

export
ext_code_block_single_line : Property
ext_code_block_single_line = oneShot $
  parseDoc "```\nhi\n```" === ok (doc [code "" "hi"])

export
ext_code_block_multi_line : Property
ext_code_block_multi_line = oneShot $
  parseDoc "```\nline1\nline2\n```"
    === ok (doc [code "" "line1\nline2"])

export
ext_code_block_with_info : Property
ext_code_block_with_info = oneShot $
  parseDoc "```idris\nfoo\n```" === ok (doc [code "idris" "foo"])

export
ext_code_block_info_trimmed : Property
ext_code_block_info_trimmed = oneShot $
  parseDoc "```   bash   \nx\n```" === ok (doc [code "bash" "x"])

||| Body preserves blank lines INSIDE the fence.
export
ext_code_block_blank_lines_inside : Property
ext_code_block_blank_lines_inside = oneShot $
  parseDoc "```\na\n\nb\n```"
    === ok (doc [code "" "a\n\nb"])

||| Closing fence must match opener length exactly.
export
ext_code_block_longer_close_fence_does_not_close : Property
ext_code_block_longer_close_fence_does_not_close = oneShot $
  parseDoc "```\nx\n````\nstill body\n```"
    === ok (doc [code "" "x\n````\nstill body"])

||| Unclosed code block auto-closes at EOF (tolerant of malformed input).
export
ext_code_block_unclosed_auto_closes : Property
ext_code_block_unclosed_auto_closes = oneShot $
  parseDoc "```\nbody\nmore"
    === ok (doc [code "" "body\nmore"])

||| 2 backticks is NOT a fence — falls through to paragraph.
export
ext_two_backticks_is_paragraph : Property
ext_two_backticks_is_paragraph = oneShot $
  parseDoc "``" === ok (doc [para "``"])

||| Backticks INSIDE the opener's info string disqualify (Djot rule).
||| Under the rule: opener line is rejected; the line becomes paragraph
||| content; only the standalone ``` line at the end opens a real (empty)
||| code block. Note the inline parser now extracts the trailing
||| `` ` ` `` as a verbatim span — Djot verbatim semantics apply even
||| inside a rejected-fence paragraph.
export
ext_info_with_backticks_is_not_fence : Property
ext_info_with_backticks_is_not_fence = oneShot $
  parseDoc "```` ` `\nbody\n```"
    === ok (doc [ paraMulti
                    [ InlText "```"
                    , InlVerbatim emptyAttrs " "
                    , InlText " `"
                    , InlSoftBreak
                    , InlText "body"
                    ]
                , code "" ""
                ])

||| A code block separates surrounding paragraphs.
export
ext_paragraph_then_code_then_paragraph : Property
ext_paragraph_then_code_then_paragraph = oneShot $
  parseDoc "before\n\n```\nin\n```\n\nafter"
    === ok (doc [para "before", code "" "in", para "after"])

codeFenceCases : List (String, Doc)
codeFenceCases =
  [ ("```\n```",            doc [code "" ""])
  , ("```\nx\n```",         doc [code "" "x"])
  , ("```js\nx\n```",       doc [code "js" "x"])
  , ("````\nx\n````",       doc [code "" "x"])
  , ("`````\nx\n`````",     doc [code "" "x"])
  ]

export
pddt_code_fence_variants : Property
pddt_code_fence_variants = withTests 1 . property $ do
  for_ codeFenceCases $ \(src, expected) =>
    parseDoc src === ok expected

||| Any well-formed `\`\`\`\\nBODY\\n\`\`\`` always yields one CodeBlock.
export
pbt_code_block_round_trip_body : Property
pbt_code_block_round_trip_body = property $ do
  let safeChar = element $ the (Vect _ Char)
        ['a','b','c','d','x','y','z','0','1','2',' ', '_']
  body <- forAll $ pack <$> list (linear 0 24) safeChar
  let src = "```\n" ++ body ++ "\n```"
  case parseDoc src of
    Right (MkDoc [CodeBlock _ "" b]) => b === body
    Right d => failWith Nothing ("expected single CodeBlock; got " ++ show d)
    Left  e => failWith Nothing (show e)

--- Block quotes -------------------------------------------------------------

export
ext_blockquote_single_line : Property
ext_blockquote_single_line = oneShot $
  parseDoc "> hi"
    === ok (doc [BlockQuote emptyAttrs [para "hi"]])

export
ext_blockquote_multi_line_paragraph : Property
ext_blockquote_multi_line_paragraph = oneShot $
  parseDoc "> line1\n> line2"
    === ok (doc [BlockQuote emptyAttrs
                   [paraMulti [InlText "line1", InlSoftBreak, InlText "line2"]]])

export
ext_blockquote_with_heading_inside : Property
ext_blockquote_with_heading_inside = oneShot $
  parseDoc "> # Title"
    === ok (doc [BlockQuote emptyAttrs [heading 1 "Title"]])

export
ext_blockquote_with_thematic_inside : Property
ext_blockquote_with_thematic_inside = oneShot $
  parseDoc "> ---"
    === ok (doc [BlockQuote emptyAttrs [ThematicBreak emptyAttrs]])

||| Nested quote: `>> x` → quote containing a quote containing "x".
export
ext_blockquote_nested : Property
ext_blockquote_nested = oneShot $
  parseDoc "> > inner"
    === ok (doc [BlockQuote emptyAttrs
                   [BlockQuote emptyAttrs [para "inner"]]])

||| `> ` followed by `>` (empty quote line) splits paragraphs INSIDE the quote.
export
ext_blockquote_empty_quote_line_separates_paragraphs : Property
ext_blockquote_empty_quote_line_separates_paragraphs = oneShot $
  parseDoc "> p1\n>\n> p2"
    === ok (doc [BlockQuote emptyAttrs [para "p1", para "p2"]])

export
ext_paragraph_then_blockquote : Property
ext_paragraph_then_blockquote = oneShot $
  parseDoc "before\n\n> quoted"
    === ok (doc [para "before", BlockQuote emptyAttrs [para "quoted"]])

||| A `>`-prefixed line directly after a paragraph (no blank line) still
||| flushes the paragraph and starts a quote group, since `>` is a structural
||| boundary not a paragraph-continuation.
export
ext_blockquote_then_paragraph_no_blank : Property
ext_blockquote_then_paragraph_no_blank = oneShot $
  parseDoc "> quoted\nafter"
    === ok (doc [BlockQuote emptyAttrs [para "quoted"], para "after"])

||| `>x` (no space) is NOT a quote — falls through to paragraph.
export
ext_greater_then_no_space_is_paragraph : Property
ext_greater_then_no_space_is_paragraph = oneShot $
  parseDoc ">x" === ok (doc [para ">x"])

quotePrefixCases : List (String, Doc)
quotePrefixCases =
  [ ("> a",           doc [BlockQuote emptyAttrs [para "a"]])
  , ("> a\n> b",      doc [BlockQuote emptyAttrs
                              [paraMulti [InlText "a", InlSoftBreak, InlText "b"]]])
  , ("> # T",         doc [BlockQuote emptyAttrs [heading 1 "T"]])
  , ("> > x",         doc [BlockQuote emptyAttrs
                              [BlockQuote emptyAttrs [para "x"]]])
  ]

export
pddt_quote_prefix_variants : Property
pddt_quote_prefix_variants = withTests 1 . property $ do
  for_ quotePrefixCases $ \(src, expected) =>
    parseDoc src === ok expected

||| Any input whose lines all start with `> ` yields a single top-level
||| BlockQuote block.
export
pbt_quote_prefixed_input_yields_blockquote_top_level : Property
pbt_quote_prefixed_input_yields_blockquote_top_level = property $ do
  let safe = element $ the (Vect _ Char)
        ['a','b','c','d','e','f','g','h','i','j','k','l','m'
        ,'n','o','p','q','r','s','t','u','v','w','x','y','z'
        ,'0','1','2','3','4','5','6','7','8','9']
  bodies <- forAll $ list (linear 1 4) $
    [| pack (list (linear 1 6) safe) |]
  let src = concat $ intersperse "\n" (map ("> " ++) bodies)
  case parseDoc src of
    Right (MkDoc [BlockQuote _ _]) => success
    Right d => failWith Nothing ("expected single BlockQuote; got " ++ show d)
    Left  e => failWith Nothing (show e)

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

--------------------------------------------------------------------------------
-- Inline emphasis / strong / verbatim / link (Step-8 parser remainder).
--------------------------------------------------------------------------------

export
ext_inline_emphasis : Property
ext_inline_emphasis = oneShot $
  parseDoc "a _b_ c"
    === ok (doc [paraMulti
                   [ InlText "a "
                   , InlEmph [InlText "b"]
                   , InlText " c"
                   ]])

export
ext_inline_strong : Property
ext_inline_strong = oneShot $
  parseDoc "a *b* c"
    === ok (doc [paraMulti
                   [ InlText "a "
                   , InlStrong [InlText "b"]
                   , InlText " c"
                   ]])

export
ext_inline_verbatim : Property
ext_inline_verbatim = oneShot $
  parseDoc "see `code` for"
    === ok (doc [paraMulti
                   [ InlText "see "
                   , InlVerbatim emptyAttrs "code"
                   , InlText " for"
                   ]])

export
ext_inline_link : Property
ext_inline_link = oneShot $
  parseDoc "see [Cribrum](/cribrum) repo"
    === ok (doc [paraMulti
                   [ InlText "see "
                   , InlLink emptyAttrs
                       (LinkInline "/cribrum" Nothing)
                       [InlText "Cribrum"]
                   , InlText " repo"
                   ]])

export
ext_inline_link_with_emphasis_label : Property
ext_inline_link_with_emphasis_label = oneShot $
  parseDoc "[_em_](u)"
    === ok (doc [paraMulti
                   [ InlLink emptyAttrs (LinkInline "u" Nothing)
                       [InlEmph [InlText "em"]]
                   ]])

export
ext_inline_unpaired_marker_is_text : Property
ext_inline_unpaired_marker_is_text = oneShot $
  parseDoc "a*b" === ok (doc [para "a*b"])

export
ext_inline_empty_emphasis_is_text : Property
ext_inline_empty_emphasis_is_text = oneShot $
  -- Empty `__`/`**`/```` between two markers does NOT trigger emphasis.
  parseDoc "__" === ok (doc [para "__"])

export
ext_inline_unpaired_link_is_text : Property
ext_inline_unpaired_link_is_text = oneShot $
  -- A `[` with no matching `]` falls back to plain text.
  parseDoc "[orphan" === ok (doc [para "[orphan"])

export
ext_inline_nested_emphasis_in_strong : Property
ext_inline_nested_emphasis_in_strong = oneShot $
  parseDoc "*outer _inner_ rest*"
    === ok (doc [paraMulti
                   [ InlStrong
                       [ InlText "outer "
                       , InlEmph [InlText "inner"]
                       , InlText " rest"
                       ]
                   ]])

--------------------------------------------------------------------------------
-- Inline images (`![alt](src)`).
--------------------------------------------------------------------------------

export
ext_inline_image_basic : Property
ext_inline_image_basic = oneShot $
  parseDoc "see ![cat](/cat.png) here"
    === ok (doc [paraMulti
                   [ InlText "see "
                   , InlImage emptyAttrs (LinkInline "/cat.png" Nothing)
                       [InlText "cat"]
                   , InlText " here"
                   ]])

||| Empty alt is permitted (Djot's decorative-image form).
export
ext_inline_image_empty_alt : Property
ext_inline_image_empty_alt = oneShot $
  parseDoc "![](/dec.png)"
    === ok (doc [paraMulti
                   [ InlImage emptyAttrs (LinkInline "/dec.png" Nothing) []
                   ]])

||| Alt content is recursively parsed for inline markup.
export
ext_inline_image_with_emphasis_alt : Property
ext_inline_image_with_emphasis_alt = oneShot $
  parseDoc "![_pic_](/x.png)"
    === ok (doc [paraMulti
                   [ InlImage emptyAttrs (LinkInline "/x.png" Nothing)
                       [InlEmph [InlText "pic"]]
                   ]])

||| Empty src is rejected (the `!` falls back to literal text).
export
ext_inline_image_empty_src_is_text : Property
ext_inline_image_empty_src_is_text = oneShot $
  parseDoc "![alt]()"
    === ok (doc [para "![alt]()"])

||| `!` not followed by a well-formed image body is literal text.
export
ext_inline_bang_alone_is_text : Property
ext_inline_bang_alone_is_text = oneShot $
  parseDoc "wow!" === ok (doc [para "wow!"])

--------------------------------------------------------------------------------
-- Autolinks (`<url>` / `<email>`).
--------------------------------------------------------------------------------

export
ext_inline_autolink_url : Property
ext_inline_autolink_url = oneShot $
  parseDoc "visit <https://example.org> today"
    === ok (doc [paraMulti
                   [ InlText "visit "
                   , InlLink emptyAttrs (LinkAuto "https://example.org")
                       [InlText "https://example.org"]
                   , InlText " today"
                   ]])

export
ext_inline_autolink_email : Property
ext_inline_autolink_email = oneShot $
  parseDoc "ping <me@example.org>"
    === ok (doc [paraMulti
                   [ InlText "ping "
                   , InlLink emptyAttrs (LinkAuto "me@example.org")
                       [InlText "me@example.org"]
                   ]])

||| `<x>` without scheme/email is treated as literal text (the simple
||| heuristic guards against eating every angle-bracketed phrase in
||| prose; stock Djot is more permissive — full conformance arrives
||| with the reference-suite gate). The unpaired `<` falls back into
||| the plain-text accumulator so the surrounding run stays a single
||| `InlText`.
export
ext_inline_angle_bracketed_word_is_text : Property
ext_inline_angle_bracketed_word_is_text = oneShot $
  parseDoc "see <foo> below"
    === ok (doc [para "see <foo> below"])

||| Whitespace inside the brackets disqualifies the autolink.
export
ext_inline_angle_with_space_is_text : Property
ext_inline_angle_with_space_is_text = oneShot $
  parseDoc "x <a b> y"
    === ok (doc [para "x <a b> y"])

||| Whitespace inside the brackets disqualifies even when the body
||| *would* otherwise pass the scheme heuristic. Pins the
||| `noWhite &&` guard against being dropped in favour of just
||| `hasColon || hasAt`.
export
ext_inline_angle_with_scheme_and_space_is_text : Property
ext_inline_angle_with_scheme_and_space_is_text = oneShot $
  parseDoc "x <foo:bar baz> y"
    === ok (doc [para "x <foo:bar baz> y"])

||| Empty body `<>` is not an autolink — pins
||| `isAutolinkBody []  = False`.
export
ext_inline_angle_empty_is_text : Property
ext_inline_angle_empty_is_text = oneShot $
  parseDoc "x <> y"
    === ok (doc [para "x <> y"])

--------------------------------------------------------------------------------
-- Hard breaks (trailing `\\` on a paragraph line).
--------------------------------------------------------------------------------

export
ext_hardbreak_between_lines : Property
ext_hardbreak_between_lines = oneShot $
  parseDoc "alpha\\\nbravo"
    === ok (doc [paraMulti
                   [ InlText "alpha"
                   , InlHardBreak
                   , InlText "bravo"
                   ]])

||| Plain soft break (no trailing `\\`) is still a SoftBreak — slice
||| does not regress prior behaviour.
export
ext_softbreak_still_default : Property
ext_softbreak_still_default = oneShot $
  parseDoc "alpha\nbravo"
    === ok (doc [paraMulti
                   [ InlText "alpha"
                   , InlSoftBreak
                   , InlText "bravo"
                   ]])

||| Trailing `\\` on the *last* line of a paragraph is left literal —
||| no following line means no hard break is meaningful.
export
ext_trailing_backslash_at_eop_is_literal : Property
ext_trailing_backslash_at_eop_is_literal = oneShot $
  parseDoc "alpha\\"
    === ok (doc [paraMulti [InlText "alpha\\"]])

||| Hard break in a three-line paragraph: mixed soft + hard breaks.
export
ext_hardbreak_mixed_with_softbreak : Property
ext_hardbreak_mixed_with_softbreak = oneShot $
  parseDoc "alpha\\\nbravo\ncharlie"
    === ok (doc [paraMulti
                   [ InlText "alpha"
                   , InlHardBreak
                   , InlText "bravo"
                   , InlSoftBreak
                   , InlText "charlie"
                   ]])

--------------------------------------------------------------------------------
-- Smart punctuation (`--`/`---`/`...`/`"`/`'`).
--------------------------------------------------------------------------------

export
ext_smart_endash : Property
ext_smart_endash = oneShot $
  parseDoc "a--b"
    === ok (doc [paraMulti
                   [InlText "a", InlSmart EnDash, InlText "b"]])

export
ext_smart_emdash : Property
ext_smart_emdash = oneShot $
  parseDoc "a---b"
    === ok (doc [paraMulti
                   [InlText "a", InlSmart EmDash, InlText "b"]])

||| Em-dash takes precedence over en-dash on a 3-dash run.
export
ext_smart_emdash_wins_over_endash : Property
ext_smart_emdash_wins_over_endash = oneShot $
  parseDoc "x---y--z"
    === ok (doc [paraMulti
                   [ InlText "x", InlSmart EmDash, InlText "y"
                   , InlSmart EnDash, InlText "z"
                   ]])

||| Four dashes = em-dash + literal `-`. Six dashes = two em-dashes.
export
ext_smart_four_dashes : Property
ext_smart_four_dashes = oneShot $
  parseDoc "a----b"
    === ok (doc [paraMulti
                   [InlText "a", InlSmart EmDash, InlText "-b"]])

export
ext_smart_ellipsis : Property
ext_smart_ellipsis = oneShot $
  parseDoc "wait... maybe"
    === ok (doc [paraMulti
                   [InlText "wait", InlSmart Ellipsis, InlText " maybe"]])

||| Two dots stay literal.
export
ext_smart_two_dots_is_literal : Property
ext_smart_two_dots_is_literal = oneShot $
  parseDoc "ok.." === ok (doc [para "ok.."])

||| Quote orientation: open after start-of-string, close after letter.
export
ext_smart_double_quote_orientation : Property
ext_smart_double_quote_orientation = oneShot $
  parseDoc "\"hello\""
    === ok (doc [paraMulti
                   [ InlSmart LDQuote
                   , InlText "hello"
                   , InlSmart RDQuote
                   ]])

export
ext_smart_single_quote_apostrophe : Property
ext_smart_single_quote_apostrophe = oneShot $
  parseDoc "Djot's"
    === ok (doc [paraMulti
                   [InlText "Djot", InlSmart RSQuote, InlText "s"]])

||| Open after whitespace, close after letter.
export
ext_smart_single_quote_pair : Property
ext_smart_single_quote_pair = oneShot $
  parseDoc "say 'hi' now"
    === ok (doc [paraMulti
                   [ InlText "say "
                   , InlSmart LSQuote
                   , InlText "hi"
                   , InlSmart RSQuote
                   , InlText " now"
                   ]])

||| Quote after `(` is opening (opening-punct context).
export
ext_smart_quote_after_open_paren_is_open : Property
ext_smart_quote_after_open_paren_is_open = oneShot $
  parseDoc "(\"a\")"
    === ok (doc [paraMulti
                   [ InlText "("
                   , InlSmart LDQuote
                   , InlText "a"
                   , InlSmart RDQuote
                   , InlText ")"
                   ]])

--------------------------------------------------------------------------------
-- Pipe tables.
--------------------------------------------------------------------------------

bodyCell : String -> TableCell
bodyCell s = MkCell AlignNone [InlText s]

bodyRow : List String -> TableRow
bodyRow xs = MkRow False (map bodyCell xs)

||| Single-row table with no alignment row -> body-only, AlignNone.
export
ext_table_single_row_no_header : Property
ext_table_single_row_no_header = oneShot $
  parseDoc "| a | b |"
    === ok (doc [Table emptyAttrs Nothing [bodyRow ["a", "b"]]])

||| Header + alignment + body. The alignment row classifies row 1 as
||| header and supplies per-column align to ALL rows (header + body).
export
ext_table_header_with_alignment : Property
ext_table_header_with_alignment = oneShot $
  parseDoc "| h1 | h2 |\n|:---|---:|\n| a  | b  |"
    === ok (doc [Table emptyAttrs Nothing
                   [ MkRow True
                       [ MkCell AlignLeft  [InlText "h1"]
                       , MkCell AlignRight [InlText "h2"]
                       ]
                   , MkRow False
                       [ MkCell AlignLeft  [InlText "a"]
                       , MkCell AlignRight [InlText "b"]
                       ]
                   ]])

||| `:--:` -> AlignCenter; bare `---` -> AlignNone in header context.
export
ext_table_all_alignments : Property
ext_table_all_alignments = oneShot $
  parseDoc "| a | b | c | d |\n|---|:---|:---:|---:|"
    === ok (doc [Table emptyAttrs Nothing
                   [ MkRow True
                       [ MkCell AlignNone   [InlText "a"]
                       , MkCell AlignLeft   [InlText "b"]
                       , MkCell AlignCenter [InlText "c"]
                       , MkCell AlignRight  [InlText "d"]
                       ]
                   ]])

||| Two-row table where the second row is NOT an alignment row -> no
||| header, both rows are body rows.
export
ext_table_two_rows_no_alignment_row : Property
ext_table_two_rows_no_alignment_row = oneShot $
  parseDoc "| a | b |\n| c | d |"
    === ok (doc [Table emptyAttrs Nothing
                   [ bodyRow ["a", "b"]
                   , bodyRow ["c", "d"]
                   ]])

||| Cells parse as inlines (emphasis applied inside a cell).
export
ext_table_cell_inline_parsing : Property
ext_table_cell_inline_parsing = oneShot $
  parseDoc "| _em_ | *strong* |"
    === ok (doc [Table emptyAttrs Nothing
                   [ MkRow False
                       [ MkCell AlignNone [InlEmph   [InlText "em"]]
                       , MkCell AlignNone [InlStrong [InlText "strong"]]
                       ]
                   ]])

||| A blank line ends the table and starts a new block.
export
ext_table_then_paragraph : Property
ext_table_then_paragraph = oneShot $
  parseDoc "| a | b |\n\nafter"
    === ok (doc [ Table emptyAttrs Nothing [bodyRow ["a", "b"]]
                , para "after"
                ])

||| A row with a leading `|` but no further `|` is NOT a table — falls
||| through to paragraph.
export
ext_single_pipe_is_paragraph : Property
ext_single_pipe_is_paragraph = oneShot $
  parseDoc "| only one"
    === ok (doc [para "| only one"])

||| Alignment-row cells need at least 3 dashes. `|--|` (2 dashes) does
||| NOT promote the previous row to a header — both rows stay body.
||| (The `--` cell content then smart-puncts to an en-dash, but it is
||| still in a `<td>` body cell, not a `<th>` header cell.) Pins the
||| `length bar >= 3` guard.
export
ext_table_two_dash_align_not_header : Property
ext_table_two_dash_align_not_header = oneShot $
  parseDoc "| h |\n|--|"
    === ok (doc [Table emptyAttrs Nothing
                   [ bodyRow ["h"]
                   , MkRow False [MkCell AlignNone [InlSmart EnDash]]
                   ]])

||| Alignment-row cells must be homogeneous dashes (with optional
||| flanking `:`). A row with non-dash content like `|xxx|` is not an
||| alignment row — both rows stay body. Pins the `all (== '-')`
||| guard.
export
ext_table_non_dash_align_not_header : Property
ext_table_non_dash_align_not_header = oneShot $
  parseDoc "| h |\n|xxx|"
    === ok (doc [Table emptyAttrs Nothing
                   [ bodyRow ["h"]
                   , bodyRow ["xxx"]
                   ]])

--------------------------------------------------------------------------------
-- Reference definitions + reference-style links.
--------------------------------------------------------------------------------

||| Standalone reference definition `[ref]: url` becomes a `RefDef`
||| block (no title).
export
ext_refdef_url_only : Property
ext_refdef_url_only = oneShot $
  parseDoc "[home]: https://example.org"
    === ok (doc [RefDef "home" "https://example.org" Nothing])

||| Reference definition with a trailing double-quoted title.
export
ext_refdef_with_title : Property
ext_refdef_with_title = oneShot $
  parseDoc "[home]: https://example.org \"Home page\""
    === ok (doc [RefDef "home" "https://example.org" (Just "Home page")])

||| Full reference link `[text][ref]` is resolved against a RefDef
||| later in the document: the `LinkReference` is rewritten to
||| `LinkInline` carrying the defined URL.
export
ext_full_ref_link_resolved : Property
ext_full_ref_link_resolved = oneShot $
  parseDoc "see [home][h] please\n\n[h]: https://example.org"
    === ok (doc
              [ paraMulti
                  [ InlText "see "
                  , InlLink emptyAttrs
                      (LinkInline "https://example.org" Nothing)
                      [InlText "home"]
                  , InlText " please"
                  ]
              , RefDef "h" "https://example.org" Nothing
              ])

||| Collapsed reference link `[text][]` uses the visible text as the
||| label.
export
ext_collapsed_ref_link_resolved : Property
ext_collapsed_ref_link_resolved = oneShot $
  parseDoc "see [home][] please\n\n[home]: https://example.org"
    === ok (doc
              [ paraMulti
                  [ InlText "see "
                  , InlLink emptyAttrs
                      (LinkInline "https://example.org" Nothing)
                      [InlText "home"]
                  , InlText " please"
                  ]
              , RefDef "home" "https://example.org" Nothing
              ])

||| Undefined reference label leaves the `LinkReference` intact (the
||| elaborator renders it as a fallback anchor `<a href="#missing">`).
export
ext_undefined_ref_link_stays_reference : Property
ext_undefined_ref_link_stays_reference = oneShot $
  parseDoc "see [home][missing] please"
    === ok (doc
              [ paraMulti
                  [ InlText "see "
                  , InlLink emptyAttrs
                      (LinkReference "missing")
                      [InlText "home"]
                  , InlText " please"
                  ]
              ])

||| Reference defined BEFORE the link still resolves (the two-pass
||| resolver doesn't care about source order).
export
ext_ref_defined_before_link_resolves : Property
ext_ref_defined_before_link_resolves = oneShot $
  parseDoc "[h]: https://example.org\n\nsee [home][h] please"
    === ok (doc
              [ RefDef "h" "https://example.org" Nothing
              , paraMulti
                  [ InlText "see "
                  , InlLink emptyAttrs
                      (LinkInline "https://example.org" Nothing)
                      [InlText "home"]
                  , InlText " please"
                  ]
              ])

||| A `[ref]:` line with no URL is NOT a RefDef — falls through to
||| paragraph (which then renders as a paragraph containing a
||| reference-style link).
export
ext_refdef_empty_url_is_paragraph : Property
ext_refdef_empty_url_is_paragraph = oneShot $
  parseDoc "[h]: "
    === ok (doc [paraMulti [InlText "[h]: "]])

||| `[ref]:url` (no space after the colon) is NOT a RefDef — the
||| `:` must be followed by a space. Pins the `(':' :: ' ' :: body)`
||| split against being relaxed to `(':' :: body)`.
export
ext_refdef_requires_space_after_colon : Property
ext_refdef_requires_space_after_colon = oneShot $
  parseDoc "[h]:url"
    === ok (doc [paraMulti [InlText "[h]:url"]])

--------------------------------------------------------------------------------
-- Footnote definitions + references (parser remainder).
--------------------------------------------------------------------------------

||| `[^lbl]` becomes an `InlFootnoteRef` in a paragraph.
export
ext_footnote_ref_inline : Property
ext_footnote_ref_inline = oneShot $
  parseDoc "see[^a]"
    === ok (doc [paraMulti [InlText "see", InlFootnoteRef "a"]])

||| `[^]` (empty label between `[^` and `]`) is NOT a footnote
||| reference — the label is required. The `[^` falls back to
||| literal text. Kills the mutant that would emit an empty-label
||| `InlFootnoteRef`.
export
ext_footnote_empty_inline_ref_is_literal : Property
ext_footnote_empty_inline_ref_is_literal = oneShot $
  parseDoc "see[^]"
    === ok (doc [paraMulti [InlText "see[^]"]])

||| `[^lbl]: body` becomes a `FootnoteDef` block, with the body inline-
||| parsed as a single paragraph.
export
ext_footnote_def_single_line : Property
ext_footnote_def_single_line = oneShot $
  parseDoc "[^a]: note body"
    === ok (doc [FootnoteDef emptyAttrs "a"
                   [paraMulti [InlText "note body"]]])

||| Indented continuation line joins the opener via `InlSoftBreak`.
export
ext_footnote_def_continuation : Property
ext_footnote_def_continuation = oneShot $
  parseDoc "[^a]: first\n  second"
    === ok (doc [FootnoteDef emptyAttrs "a"
                   [paraMulti
                     [InlText "first", InlSoftBreak, InlText "second"]]])

||| Footnote body can contain multiple paragraphs separated by a
||| blank line; the indented continuation pattern keeps them inside the
||| same `FootnoteDef`.
export
ext_footnote_def_two_paragraphs : Property
ext_footnote_def_two_paragraphs = oneShot $
  parseDoc "[^a]: first\n\n  second"
    === ok (doc [FootnoteDef emptyAttrs "a"
                   [paraMulti [InlText "first"]
                   , paraMulti [InlText "second"]]])

||| `{#id .cls}\nfoo` attaches Attrs to the following paragraph.
export
ext_attr_block_prefixes_paragraph : Property
ext_attr_block_prefixes_paragraph = oneShot $
  parseDoc "{#id .cls}\nfoo"
    === ok (doc [Paragraph (MkAttrs (Just "id") ["cls"] [])
                            [InlText "foo"]])

||| Multiple consecutive `{...}` lines stack — classes append; id /
||| key=val take last value.
export
ext_attr_blocks_stack : Property
ext_attr_blocks_stack = oneShot $
  parseDoc "{#id}\n{.foo .bar}\n{#id2}\nOkay"
    === ok (doc [Paragraph (MkAttrs (Just "id2") ["foo","bar"] [])
                            [InlText "Okay"]])

||| Trailing attribute prefix with no following block is dropped.
export
ext_trailing_attr_block_dropped : Property
ext_trailing_attr_block_dropped = oneShot $
  parseDoc "para\n\n{#id}"
    === ok (doc [Paragraph emptyAttrs [InlText "para"]])

||| Empty-label `[^]:` is NOT a footnote opener — `parseFootnoteOpener`
||| requires a non-empty label. The line then falls through to the
||| normal ref-def path, which accepts `^` as a label. (Documents this
||| split; not a deep design commitment — the elaborator never emits
||| a useful link for label `^`.)
export
ext_footnote_empty_label_is_refdef : Property
ext_footnote_empty_label_is_refdef = oneShot $
  parseDoc "[^]: ignored"
    === ok (doc [RefDef "^" "ignored" Nothing])

--------------------------------------------------------------------------------
-- Unordered & ordered list parsing (Step-8 parser remainder).
--------------------------------------------------------------------------------

ulList : List String -> Block
ulList items =
  ListBlock emptyAttrs UnorderedDash Nothing True
    (map (\s => MkLI emptyAttrs Nothing Nothing
                     [Paragraph emptyAttrs [InlText s]])
         items)

export
ext_single_item_unordered_list : Property
ext_single_item_unordered_list = oneShot $
  parseDoc "- alpha"
    === ok (doc [ulList ["alpha"]])

export
ext_multi_item_unordered_list : Property
ext_multi_item_unordered_list = oneShot $
  parseDoc "- alpha\n- bravo\n- charlie"
    === ok (doc [ulList ["alpha", "bravo", "charlie"]])

export
ext_unordered_list_then_paragraph : Property
ext_unordered_list_then_paragraph = oneShot $
  parseDoc "- one\n- two\n\nafter"
    === ok (doc [ ulList ["one", "two"]
                , para "after"
                ])

export
ext_unordered_list_dash_dash_dash : Property
ext_unordered_list_dash_dash_dash = oneShot $
  -- "- - -" is a thematic break per Djot (3+ dash marks separated
  -- by optional whitespace, alone on the line). Pins the parser
  -- against regressing the loose-dash thematic-break form to a
  -- one-item list (the prior behaviour, before the
  -- `filter (not . isSpace)` relaxation in `isThematicBreak`).
  parseDoc "- - -"
    === ok (doc [ThematicBreak emptyAttrs])

export
ext_mixed_markers_break_list : Property
ext_mixed_markers_break_list = oneShot $
  -- A run mixing `-` and `*` markers in adjacent lines breaks list
  -- grouping back to paragraph (Djot disallows mixed-marker runs).
  parseDoc "- one\n* two"
    === ok (doc [paraMulti
                   [ InlText "- one"
                   , InlSoftBreak
                   , InlText "* two"
                   ]])

export
ext_ordered_decimal_list : Property
ext_ordered_decimal_list = oneShot $
  parseDoc "1. alpha\n2. bravo"
    === ok (doc [ListBlock emptyAttrs OrderedDecimal Nothing True
                   [ MkLI emptyAttrs Nothing Nothing
                       [Paragraph emptyAttrs [InlText "alpha"]]
                   , MkLI emptyAttrs Nothing Nothing
                       [Paragraph emptyAttrs [InlText "bravo"]]
                   ]])

export
ext_unordered_list_with_inline_emphasis : Property
ext_unordered_list_with_inline_emphasis = oneShot $
  parseDoc "- a _b_ c"
    === ok (doc [ListBlock emptyAttrs UnorderedDash Nothing True
                   [ MkLI emptyAttrs Nothing Nothing
                       [Paragraph emptyAttrs
                          [ InlText "a "
                          , InlEmph [InlText "b"]
                          , InlText " c"
                          ]]
                   ]])

--------------------------------------------------------------------------------
-- Task lists + definition lists (P5.4 remainder slice).
--------------------------------------------------------------------------------

export
ext_task_list_unchecked_then_checked : Property
ext_task_list_unchecked_then_checked = oneShot $
  parseDoc "- [ ] one\n- [x] two"
    === ok (doc [ListBlock emptyAttrs TaskList Nothing True
                   [ MkLI emptyAttrs (Just False) Nothing
                       [Paragraph emptyAttrs [InlText "one"]]
                   , MkLI emptyAttrs (Just True) Nothing
                       [Paragraph emptyAttrs [InlText "two"]]
                   ]])

export
ext_task_list_uppercase_x_is_checked : Property
ext_task_list_uppercase_x_is_checked = oneShot $
  parseDoc "* [X] done"
    === ok (doc [ListBlock emptyAttrs TaskList Nothing True
                   [ MkLI emptyAttrs (Just True) Nothing
                       [Paragraph emptyAttrs [InlText "done"]]
                   ]])

export
ext_definition_list_basic : Property
ext_definition_list_basic = oneShot $
  parseDoc ": apple\n\n  red fruit\n: banana\n\n  yellow fruit"
    === ok (doc [ListBlock emptyAttrs Definition Nothing False
                   [ MkLI emptyAttrs Nothing
                       (Just [InlText "apple"])
                       [Paragraph emptyAttrs [InlText "red fruit"]]
                   , MkLI emptyAttrs Nothing
                       (Just [InlText "banana"])
                       [Paragraph emptyAttrs [InlText "yellow fruit"]]
                   ]])

export
ext_definition_list_term_only : Property
ext_definition_list_term_only = oneShot $
  -- An opener with no body produces an item whose `term` is the
  -- inline content and whose `content` is empty.
  parseDoc ": orange"
    === ok (doc [ListBlock emptyAttrs Definition Nothing False
                   [ MkLI emptyAttrs Nothing
                       (Just [InlText "orange"])
                       []
                   ]])

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
  , ("ext_code_block_empty",                     ext_code_block_empty)
  , ("ext_code_block_single_line",               ext_code_block_single_line)
  , ("ext_code_block_multi_line",                ext_code_block_multi_line)
  , ("ext_code_block_with_info",                 ext_code_block_with_info)
  , ("ext_code_block_info_trimmed",              ext_code_block_info_trimmed)
  , ("ext_code_block_blank_lines_inside",        ext_code_block_blank_lines_inside)
  , ("ext_code_block_longer_close_fence_does_not_close",
        ext_code_block_longer_close_fence_does_not_close)
  , ("ext_code_block_unclosed_auto_closes",      ext_code_block_unclosed_auto_closes)
  , ("ext_two_backticks_is_paragraph",           ext_two_backticks_is_paragraph)
  , ("ext_info_with_backticks_is_not_fence",     ext_info_with_backticks_is_not_fence)
  , ("ext_paragraph_then_code_then_paragraph",   ext_paragraph_then_code_then_paragraph)
  , ("pddt_code_fence_variants",                 pddt_code_fence_variants)
  , ("pbt_code_block_round_trip_body",           pbt_code_block_round_trip_body)
  , ("ext_blockquote_single_line",               ext_blockquote_single_line)
  , ("ext_blockquote_multi_line_paragraph",      ext_blockquote_multi_line_paragraph)
  , ("ext_blockquote_with_heading_inside",       ext_blockquote_with_heading_inside)
  , ("ext_blockquote_with_thematic_inside",      ext_blockquote_with_thematic_inside)
  , ("ext_blockquote_nested",                    ext_blockquote_nested)
  , ("ext_blockquote_empty_quote_line_separates_paragraphs",
        ext_blockquote_empty_quote_line_separates_paragraphs)
  , ("ext_paragraph_then_blockquote",            ext_paragraph_then_blockquote)
  , ("ext_blockquote_then_paragraph_no_blank",   ext_blockquote_then_paragraph_no_blank)
  , ("ext_greater_then_no_space_is_paragraph",   ext_greater_then_no_space_is_paragraph)
  , ("pddt_quote_prefix_variants",               pddt_quote_prefix_variants)
  , ("pbt_quote_prefixed_input_yields_blockquote_top_level",
        pbt_quote_prefixed_input_yields_blockquote_top_level)
  , ("pddt_heading_levels",                      pddt_heading_levels)
  , ("pddt_non_heading_levels",                  pddt_non_heading_levels)
  , ("pddt_blank_inputs",                        pddt_blank_inputs)
  , ("pbt_parser_total",                         pbt_parser_total)
  , ("pbt_safe_single_line_is_paragraph",        pbt_safe_single_line_is_paragraph)
  , ("pbt_block_count_eq_group_count",           pbt_block_count_eq_group_count)
  , ("ext_inline_emphasis",                      ext_inline_emphasis)
  , ("ext_inline_strong",                        ext_inline_strong)
  , ("ext_inline_verbatim",                      ext_inline_verbatim)
  , ("ext_inline_link",                          ext_inline_link)
  , ("ext_inline_link_with_emphasis_label",      ext_inline_link_with_emphasis_label)
  , ("ext_inline_unpaired_marker_is_text",       ext_inline_unpaired_marker_is_text)
  , ("ext_inline_empty_emphasis_is_text",        ext_inline_empty_emphasis_is_text)
  , ("ext_inline_unpaired_link_is_text",         ext_inline_unpaired_link_is_text)
  , ("ext_inline_nested_emphasis_in_strong",     ext_inline_nested_emphasis_in_strong)
  , ("ext_single_item_unordered_list",           ext_single_item_unordered_list)
  , ("ext_multi_item_unordered_list",            ext_multi_item_unordered_list)
  , ("ext_unordered_list_then_paragraph",        ext_unordered_list_then_paragraph)
  , ("ext_unordered_list_dash_dash_dash",        ext_unordered_list_dash_dash_dash)
  , ("ext_mixed_markers_break_list",             ext_mixed_markers_break_list)
  , ("ext_ordered_decimal_list",                 ext_ordered_decimal_list)
  , ("ext_unordered_list_with_inline_emphasis",  ext_unordered_list_with_inline_emphasis)
  , ("ext_task_list_unchecked_then_checked",     ext_task_list_unchecked_then_checked)
  , ("ext_task_list_uppercase_x_is_checked",     ext_task_list_uppercase_x_is_checked)
  , ("ext_definition_list_basic",                ext_definition_list_basic)
  , ("ext_definition_list_term_only",            ext_definition_list_term_only)
  , ("ext_inline_image_basic",                   ext_inline_image_basic)
  , ("ext_inline_image_empty_alt",               ext_inline_image_empty_alt)
  , ("ext_inline_image_with_emphasis_alt",       ext_inline_image_with_emphasis_alt)
  , ("ext_inline_image_empty_src_is_text",       ext_inline_image_empty_src_is_text)
  , ("ext_inline_bang_alone_is_text",            ext_inline_bang_alone_is_text)
  , ("ext_inline_autolink_url",                  ext_inline_autolink_url)
  , ("ext_inline_autolink_email",                ext_inline_autolink_email)
  , ("ext_inline_angle_bracketed_word_is_text",  ext_inline_angle_bracketed_word_is_text)
  , ("ext_inline_angle_with_space_is_text",      ext_inline_angle_with_space_is_text)
  , ("ext_inline_angle_with_scheme_and_space_is_text",
        ext_inline_angle_with_scheme_and_space_is_text)
  , ("ext_inline_angle_empty_is_text",           ext_inline_angle_empty_is_text)
  , ("ext_hardbreak_between_lines",              ext_hardbreak_between_lines)
  , ("ext_softbreak_still_default",              ext_softbreak_still_default)
  , ("ext_trailing_backslash_at_eop_is_literal", ext_trailing_backslash_at_eop_is_literal)
  , ("ext_hardbreak_mixed_with_softbreak",       ext_hardbreak_mixed_with_softbreak)
  , ("ext_smart_endash",                         ext_smart_endash)
  , ("ext_smart_emdash",                         ext_smart_emdash)
  , ("ext_smart_emdash_wins_over_endash",        ext_smart_emdash_wins_over_endash)
  , ("ext_smart_four_dashes",                    ext_smart_four_dashes)
  , ("ext_smart_ellipsis",                       ext_smart_ellipsis)
  , ("ext_smart_two_dots_is_literal",            ext_smart_two_dots_is_literal)
  , ("ext_smart_double_quote_orientation",       ext_smart_double_quote_orientation)
  , ("ext_smart_single_quote_apostrophe",        ext_smart_single_quote_apostrophe)
  , ("ext_smart_single_quote_pair",              ext_smart_single_quote_pair)
  , ("ext_smart_quote_after_open_paren_is_open", ext_smart_quote_after_open_paren_is_open)
  , ("ext_table_single_row_no_header",           ext_table_single_row_no_header)
  , ("ext_table_header_with_alignment",          ext_table_header_with_alignment)
  , ("ext_table_all_alignments",                 ext_table_all_alignments)
  , ("ext_table_two_rows_no_alignment_row",      ext_table_two_rows_no_alignment_row)
  , ("ext_table_cell_inline_parsing",            ext_table_cell_inline_parsing)
  , ("ext_table_then_paragraph",                 ext_table_then_paragraph)
  , ("ext_single_pipe_is_paragraph",             ext_single_pipe_is_paragraph)
  , ("ext_table_two_dash_align_not_header",      ext_table_two_dash_align_not_header)
  , ("ext_table_non_dash_align_not_header",      ext_table_non_dash_align_not_header)
  , ("ext_refdef_url_only",                      ext_refdef_url_only)
  , ("ext_refdef_with_title",                    ext_refdef_with_title)
  , ("ext_full_ref_link_resolved",               ext_full_ref_link_resolved)
  , ("ext_collapsed_ref_link_resolved",          ext_collapsed_ref_link_resolved)
  , ("ext_undefined_ref_link_stays_reference",   ext_undefined_ref_link_stays_reference)
  , ("ext_ref_defined_before_link_resolves",     ext_ref_defined_before_link_resolves)
  , ("ext_refdef_empty_url_is_paragraph",        ext_refdef_empty_url_is_paragraph)
  , ("ext_refdef_requires_space_after_colon",    ext_refdef_requires_space_after_colon)
  , ("ext_footnote_ref_inline",                  ext_footnote_ref_inline)
  , ("ext_footnote_empty_inline_ref_is_literal", ext_footnote_empty_inline_ref_is_literal)
  , ("ext_footnote_def_single_line",             ext_footnote_def_single_line)
  , ("ext_footnote_def_continuation",            ext_footnote_def_continuation)
  , ("ext_footnote_def_two_paragraphs",          ext_footnote_def_two_paragraphs)
  , ("ext_footnote_empty_label_is_refdef",       ext_footnote_empty_label_is_refdef)
  , ("ext_attr_block_prefixes_paragraph",        ext_attr_block_prefixes_paragraph)
  , ("ext_attr_blocks_stack",                    ext_attr_blocks_stack)
  , ("ext_trailing_attr_block_dropped",          ext_trailing_attr_block_dropped)
  ]
