||| Native Djot parser — Phase 1a.
|||
||| First slice covers paragraph + ATX-style heading (`#` x1-6 + space). The
||| full construct inventory will land incrementally; each addition arrives
||| with EXT/PDDT/PBT coverage and zero surviving mutants on its slice.
|||
||| Parser is total and pure: `parseDoc : String -> Either ParseError Doc`.
||| (Djot is structured enough that the surface parse cannot fail at this
||| slice; `ParseError` exists so future constructs can produce located
||| diagnostics without changing the signature.)
module Cribrum.Djot.Parser

import Data.List
import Data.List1
import Data.String
import Cribrum.Djot.Surface

%default total

--------------------------------------------------------------------------------
-- Errors (placeholder; full diagnostics arrive with later constructs).
--------------------------------------------------------------------------------

public export
record ParseError where
  constructor MkParseError
  line    : Nat
  column  : Nat
  message : String

public export
Show ParseError where
  show (MkParseError l c m) =
    "ParseError " ++ show l ++ ":" ++ show c ++ " " ++ m

public export
Eq ParseError where
  (MkParseError a b c) == (MkParseError x y z) = a == x && b == y && c == z

--------------------------------------------------------------------------------
-- Line classification.
--------------------------------------------------------------------------------

||| `True` if every character is whitespace.
isBlankLine : String -> Bool
isBlankLine s = all isSpace (unpack s)

||| Count leading '#' characters in a string.
countHashes : String -> Nat
countHashes = go 0 . unpack
  where
    go : Nat -> List Char -> Nat
    go n ('#' :: cs) = go (S n) cs
    go n _           = n

||| If `s` is an ATX heading marker (1..6 '#' followed by space), return
||| `Just (level, rest-without-leading-space)`. Otherwise `Nothing`.
|||
||| Djot rule (per syntax.html): 1-6 `#`, then a single space, then content.
||| `####### x` is *not* a heading — it falls through to paragraph.
parseHeadingMarker : String -> Maybe (Nat, String)
parseHeadingMarker s =
  let chars = unpack s
      n     = countHashes s
   in if n >= 1 && n <= 6
        then case drop n chars of
               (' ' :: rest) => Just (n, pack rest)
               _             => Nothing
        else Nothing

--------------------------------------------------------------------------------
-- Inline parser. Slice covers plain text + emphasis (`_em_`), strong
-- (`*strong*`), verbatim (`\`code\``), and inline links (`[text](url)`).
-- Smart-punctuation, images, footnotes, autolinks, and reference links
-- arrive in later slices.
--
-- Tokenisation strategy: scan a character list left-to-right with an
-- accumulator of "pending plain characters" that flushes to one
-- `InlText` whenever a recognised marker pairs successfully. An
-- unpaired or empty-inner marker simply joins the accumulator as a
-- literal character. This avoids inline fragmentation (one `InlText`
-- per plain run) and keeps `**` etc. as paragraphs of plain text.
--
-- Termination: each recursive descent runs on a strictly shorter
-- character list (either the suffix after a paired marker or the
-- contents *between* paired markers).
--------------------------------------------------------------------------------

||| Scan `cs` for the first occurrence of `target`. Returns
|||   `Just (inside, rest)` — characters up to but not including the closer
|||                            and the characters after it
|||   `Nothing`             — no closer
findClose : Char -> List Char -> Maybe (List Char, List Char)
findClose _ []        = Nothing
findClose c (x :: xs) =
  if x == c
    then Just ([], xs)
    else case findClose c xs of
      Just (ins, rest) => Just (x :: ins, rest)
      Nothing          => Nothing

||| Flush an accumulator of plain characters to an `InlText` (singleton
||| or empty). The accumulator is held in reverse order; flushing
||| reverses + packs.
flushAcc : List Char -> List Inline
flushAcc []  = []
flushAcc acc = [InlText (pack (reverse acc))]

mutual
  ||| Parse the body inside `[...]` and the matching `(...)` URL. Returns
  ||| an `InlLink` plus the rest of the input on success, `Nothing` on
  ||| malformed link (missing `]`, empty body, no following `(`, missing
  ||| `)`, or empty URL).
  parseLinkBody : List Char -> Maybe (Inline, List Char)
  parseLinkBody chars = case findClose ']' chars of
    Just (label, afterClose) =>
      if label == []
        then Nothing
        else case afterClose of
          ('(' :: rest) => case findClose ')' rest of
            Just (url, after) =>
              if url == []
                then Nothing
                else
                  let inner = assert_total (parseInlines label)
                   in Just (InlLink emptyAttrs
                              (LinkInline (pack url) Nothing) inner
                          , after)
            Nothing => Nothing
          _ => Nothing
    Nothing => Nothing

  ||| Drive the tokenizer with a plain-character accumulator. `acc` is
  ||| the reversed list of plain characters scanned so far in the
  ||| current run; pairs flush it, unpaired markers append to it.
  parseInlinesAcc : List Char -> List Char -> List Inline
  parseInlinesAcc acc [] = flushAcc acc
  parseInlinesAcc acc (c :: cs) = case c of
    '_' => case findClose '_' cs of
      Just (inner, after) =>
        if inner == []
          then assert_total (parseInlinesAcc ('_' :: acc) cs)
          else flushAcc acc
            ++ [InlEmph (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('_' :: acc) cs)
    '*' => case findClose '*' cs of
      Just (inner, after) =>
        if inner == []
          then assert_total (parseInlinesAcc ('*' :: acc) cs)
          else flushAcc acc
            ++ [InlStrong (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('*' :: acc) cs)
    '`' => case findClose '`' cs of
      Just (inner, after) =>
        if inner == []
          then assert_total (parseInlinesAcc ('`' :: acc) cs)
          else flushAcc acc
            ++ [InlVerbatim emptyAttrs (pack inner)]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('`' :: acc) cs)
    '[' => case parseLinkBody cs of
      Just (link, after) =>
        flushAcc acc ++ [link]
          ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('[' :: acc) cs)
    other => assert_total (parseInlinesAcc (other :: acc) cs)

  ||| Top-level inline tokenizer over character lists.
  public export
  parseInlines : List Char -> List Inline
  parseInlines chars = parseInlinesAcc [] chars

||| Parse a single line's inline content. Empty string yields no inlines;
||| otherwise the inline tokenizer runs over the line's characters.
public export
parseInlineLine : String -> List Inline
parseInlineLine "" = []
parseInlineLine s  = parseInlines (unpack s)

||| Parse a paragraph body: consecutive non-blank lines joined by SoftBreaks.
parseParagraphLines : List1 String -> List Inline
parseParagraphLines (l ::: ls) = case ls of
  [] => parseInlineLine l
  _  => parseInlineLine l ++ concatMap softThenLine ls
  where
    softThenLine : String -> List Inline
    softThenLine x = InlSoftBreak :: parseInlineLine x

--------------------------------------------------------------------------------
-- Block grouping over the raw lines.
--
-- LineGroup is the intermediate that lets parsing handle constructs that
-- span blank lines (block quote — and, in future slices, code blocks /
-- fenced divs / lists). NormalGroup is a paragraph-like run of contiguous
-- non-blank lines; QuoteGroup contains the LineGroups of its stripped
-- interior so the inner structure is parsed recursively.
--------------------------------------------------------------------------------

public export
data LineGroup : Type where
  NormalGroup : List1 String        -> LineGroup
  QuoteGroup  : List LineGroup      -> LineGroup
  CodeGroup   : (info : String) -> (body : String) -> LineGroup

||| `True` iff the line starts with `>` followed by space, OR is exactly `>`
||| (an empty quote line — Djot allows this).
isQuotePrefixed : String -> Bool
isQuotePrefixed s = case unpack s of
  ('>' :: ' ' :: _) => True
  ['>']             => True
  _                 => False

||| Count leading backticks in a string.
countBackticks : String -> Nat
countBackticks = go 0 . unpack
  where
    go : Nat -> List Char -> Nat
    go n ('`' :: cs) = go (S n) cs
    go n _           = n

||| If `s` is a fenced code-block opening line — `\`\`\`` (3+) optionally
||| followed by an info string with NO further backticks — return
||| `Just (fenceLen, info)`. Otherwise `Nothing`.
|||
||| Djot spec: opener is 3+ backticks; info string is the rest of the line
||| (trimmed); info must not contain backticks.
parseCodeFenceOpen : String -> Maybe (Nat, String)
parseCodeFenceOpen s =
  let n     = countBackticks s
      after = pack (drop n (unpack s))
   in if n >= 3 && not (any (== '`') (unpack after))
        then Just (n, trim after)
        else Nothing

||| `True` if `s` is a CLOSING fence of length `n`: exactly `n` backticks
||| (and only whitespace afterwards).
isCodeFenceClose : Nat -> String -> Bool
isCodeFenceClose n s =
  let trimmed = trim s
      bs      = countBackticks trimmed
   in bs == n && length (unpack trimmed) == n

||| Strip the leading `> ` (or just `>`) prefix, returning the inner line.
stripQuotePrefix : String -> String
stripQuotePrefix s = case unpack s of
  ('>' :: ' ' :: rest) => pack rest
  ['>']                => ""
  _                    => s

||| Take the longest prefix of `xs` that satisfies `p`. Returns
||| `(taken, rest)`.
spanList : (a -> Bool) -> List a -> (List a, List a)
spanList _ []        = ([], [])
spanList p (x :: xs) =
  if p x
    then let (ts, rs) = spanList p xs in (x :: ts, rs)
    else ([], x :: xs)

||| Group consecutive non-blank lines + recognise block-quote runs and
||| fenced code blocks.
|||
||| - Blank lines outside a quote/code run are separators.
||| - A quote run is the longest prefix of `>`-prefixed lines; the stripped
|||   interior is recursively grouped.
||| - A code fence opens with 3+ backticks and consumes EVERY following line
|||   (including blanks) until the matching closing fence. If EOF arrives
|||   before a closing fence, the block is auto-closed at EOF (matches the
|||   reference Djot implementation's tolerance).
public export
groupLines : List String -> List LineGroup
groupLines xs = go [] [] xs
  where
    flushNormal : List String -> List LineGroup -> List LineGroup
    flushNormal []           acc = acc
    flushNormal (l :: ls)    acc = NormalGroup (l ::: ls) :: acc

    -- Consume body lines until the closing fence; return (body, rest).
    collectCodeBody :
         (fenceLen : Nat)
      -> (body : List String)
      -> (rest : List String)
      -> (List String, List String)
    collectCodeBody _ body [] = (reverse body, [])
    collectCodeBody n body (l :: ls) =
      if isCodeFenceClose n l
        then (reverse body, ls)
        else collectCodeBody n (l :: body) ls

    go : (cur : List String) -> (acc : List LineGroup)
       -> List String -> List LineGroup
    go cur acc []        = reverse (flushNormal (reverse cur) acc)
    go cur acc (x :: xs) =
      if isBlankLine x
        then go [] (flushNormal (reverse cur) acc) xs
        else case parseCodeFenceOpen x of
          Just (n, info) =>
            let (body, rest) = collectCodeBody n [] xs
                acc'         = flushNormal (reverse cur) acc
                code         =
                  CodeGroup info (concat (intersperse "\n" body))
             in assert_total (go [] (code :: acc') rest)
          Nothing =>
            if isQuotePrefixed x
              then
                let (quoteLines, rest) =
                      spanList isQuotePrefixed (x :: xs)
                    inner   = map stripQuotePrefix quoteLines
                    acc'    = flushNormal (reverse cur) acc
                    quoted  = QuoteGroup (assert_total (groupLines inner))
                 in assert_total (go [] (quoted :: acc') rest)
              else go (x :: cur) acc xs

--------------------------------------------------------------------------------
-- Group -> Block.
--------------------------------------------------------------------------------

||| Per Djot spec: a thematic break is a line consisting of three or more
||| `-` or `*` characters, optionally separated by whitespace, alone on the
||| line. (We don't yet handle the mixed-whitespace form `- - -` — that is a
||| later slice; pin the simple `---`/`***` form here.)
isThematicBreak : String -> Bool
isThematicBreak s =
  let trimmed = trim s
   in case unpack trimmed of
        []        => False
        (c :: cs) =>
          (c == '-' || c == '*')
            && length (c :: cs) >= 3
            && all (== c) cs

--------------------------------------------------------------------------------
-- List parsing. The slice covers unordered lists with `-`, `*`, `+`
-- markers and ordered lists with `<n>. ` decimal markers. Nested lists,
-- task lists, definition lists, and continuation-line indentation
-- arrive in later slices.
--------------------------------------------------------------------------------

||| Recognise a list-item line. Returns `(style, body)` where `body` is
||| the inline content (everything after the marker + space).
isListLine : String -> Maybe (ListStyle, String)
isListLine s = case unpack s of
  ('-' :: ' ' :: rest) => Just (UnorderedDash,     pack rest)
  ('*' :: ' ' :: rest) => Just (UnorderedAsterisk, pack rest)
  ('+' :: ' ' :: rest) => Just (UnorderedPlus,     pack rest)
  cs                   => case parseOrderedMarker cs of
    Just (digits, body) => Just (OrderedDecimal, pack body)
    Nothing             => Nothing
  where
    -- Consume one or more digits followed by ". ". Returns
    -- `(digits, rest-after-space)` on success.
    spanDigits : List Char -> (List Char, List Char)
    spanDigits []        = ([], [])
    spanDigits (c :: cs) =
      if c >= '0' && c <= '9'
        then let (ds, r) = spanDigits cs in (c :: ds, r)
        else ([], c :: cs)

    parseOrderedMarker : List Char -> Maybe (List Char, List Char)
    parseOrderedMarker chars = case spanDigits chars of
      ([], _)               => Nothing
      (digits, '.' :: ' ' :: rest) => Just (digits, rest)
      _                     => Nothing

||| Pair a list-item line with its (style, parsed content). Each item
||| holds a single `Paragraph` of the line's inline content. Multi-line
||| / loose / nested items arrive in later slices.
parseListItem : String -> Maybe (ListStyle, ListItem)
parseListItem line = case isListLine line of
  Just (style, body) =>
    Just (style, MkLI emptyAttrs Nothing Nothing
                      [Paragraph emptyAttrs (parseInlineLine body)])
  Nothing => Nothing

||| Try to build a `ListBlock` from a run of consecutive lines. All lines
||| must be list items AND share the same style (per Djot, mixed markers
||| break the run). Returns `Nothing` if the first line isn't a list
||| item OR any subsequent line doesn't match.
tryParseList : List1 String -> Maybe Block
tryParseList (l ::: ls) = do
  (style, firstItem) <- parseListItem l
  rest <- traverse (matchSameStyle style) ls
  pure (ListBlock emptyAttrs style Nothing True (firstItem :: rest))
  where
    matchSameStyle : ListStyle -> String -> Maybe ListItem
    matchSameStyle want line = do
      (got, item) <- parseListItem line
      if got == want then Just item else Nothing

||| Convert one NORMAL line group into a block.
|||
||| Order matters: thematic break is checked first (a single `---` line is
||| not a heading and not a paragraph). Heading is checked next; then
||| list block; everything else falls through to paragraph.
normalGroupToBlock : List1 String -> Block
normalGroupToBlock (l ::: ls) =
  if isNil ls && isThematicBreak l
    then ThematicBreak emptyAttrs
    else case parseHeadingMarker l of
           Just (lvl, rest) =>
             if isNil ls
               then Heading emptyAttrs lvl (parseInlineLine rest)
               else Paragraph emptyAttrs (parseParagraphLines (l ::: ls))
           Nothing => case tryParseList (l ::: ls) of
             Just listBlock => listBlock
             Nothing        => Paragraph emptyAttrs (parseParagraphLines (l ::: ls))

||| Convert a LineGroup into a Block. Quote groups recurse.
public export
groupToBlock : LineGroup -> Block
groupToBlock (NormalGroup g)    = normalGroupToBlock g
groupToBlock (QuoteGroup  gs)   =
  BlockQuote emptyAttrs (assert_total (map groupToBlock gs))
groupToBlock (CodeGroup info b) =
  CodeBlock emptyAttrs info b

--------------------------------------------------------------------------------
-- Top-level.
--------------------------------------------------------------------------------

||| Parse a Djot document. Total; never fails on the current slice's input
||| classes (paragraph + heading). Returns `Either` so future constructs can
||| produce located errors without changing the signature.
public export
parseDoc : String -> Either ParseError Doc
parseDoc src =
  let ls = lines src
   in Right (MkDoc (map groupToBlock (groupLines ls)))
