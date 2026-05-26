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
-- (`*strong*`), verbatim (`\`code\``), inline links (`[text](url)`),
-- inline images (`![alt](src)`), autolinks (`<url>` / `<email>`), and
-- smart punctuation (`--`/`---`/`...`/`"`/`'`). Footnotes and
-- reference links arrive in later slices.
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

||| `True` iff a smart quote at this point should open (left-curly) vs
||| close (right-curly). Open if there is no preceding character (start
||| of the inline run) or the most-recently-seen character is
||| whitespace or an opening punctuation form (`(`, `[`, `{`).
|||
||| `acc` is the reversed list of plain characters already buffered, so
||| `head acc` is the most-recently-seen character.
isOpenContext : List Char -> Bool
isOpenContext []        = True
isOpenContext (c :: _)  =
  isSpace c || c == '(' || c == '[' || c == '{'

||| `True` iff the angle-bracketed body is a plausible Djot autolink:
||| non-empty, contains no whitespace, and looks like a URL (contains a
||| `:` scheme separator) OR an email (contains `@`). Stock Djot is more
||| permissive on the URL form (any scheme is accepted); the heuristic
||| here keeps the slice tight enough to avoid swallowing every `<x>`
||| literal in prose.
isAutolinkBody : List Char -> Bool
isAutolinkBody []   = False
isAutolinkBody body =
  let noWhite = not (any isSpace body)
      hasColon = any (== ':') body
      hasAt    = any (== '@') body
   in noWhite && (hasColon || hasAt)

mutual
  ||| Parse the body inside `[...]` and the matching `(...)` URL or
  ||| `[...]` reference label. Returns an `InlLink` plus the rest of
  ||| the input on success, `Nothing` on malformed link (missing `]`,
  ||| empty body, no following `(` or `[`, missing `)`/`]`, or empty
  ||| URL for the inline form).
  |||
  ||| Three link forms supported:
  |||
  |||   `[text](url)`    -> InlLink (LinkInline url Nothing)
  |||   `[text][ref]`    -> InlLink (LinkReference ref)   (full reference)
  |||   `[text][]`       -> InlLink (LinkReference text)  (collapsed reference)
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
          ('[' :: rest) => case findClose ']' rest of
            Just (refLabel, after) =>
              -- Empty ref body = collapsed form; the visible text
              -- doubles as the reference label.
              let label' = if refLabel == []
                             then pack label
                             else pack refLabel
                  inner  = assert_total (parseInlines label)
               in Just ( InlLink emptyAttrs (LinkReference label') inner
                       , after)
            Nothing => Nothing
          _ => Nothing
    Nothing => Nothing

  ||| Parse the body inside `[...]` and the matching `(...)` URL for an
  ||| inline image (`![alt](src)`). Same shape as `parseLinkBody` — the
  ||| `!` prefix is consumed by the caller. Empty alt is allowed (Djot
  ||| permits `![](url)` for decorative images); missing src or
  ||| malformed link bodies fall back so the `!` becomes literal text.
  parseImageBody : List Char -> Maybe (Inline, List Char)
  parseImageBody chars = case findClose ']' chars of
    Just (label, afterClose) => case afterClose of
      ('(' :: rest) => case findClose ')' rest of
        Just (url, after) =>
          if url == []
            then Nothing
            else
              let inner = assert_total (parseInlines label)
               in Just ( InlImage emptyAttrs
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
    '!' => case cs of
      ('[' :: rest) => case parseImageBody rest of
        Just (img, after) =>
          flushAcc acc ++ [img]
            ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('!' :: acc) cs)
      _ => assert_total (parseInlinesAcc ('!' :: acc) cs)
    '<' => case findClose '>' cs of
      Just (inner, after) =>
        if isAutolinkBody inner
          then
            let url = pack inner
             in flushAcc acc
                  ++ [InlLink emptyAttrs (LinkAuto url) [InlText url]]
                  ++ assert_total (parseInlinesAcc [] after)
          else assert_total (parseInlinesAcc ('<' :: acc) cs)
      Nothing => assert_total (parseInlinesAcc ('<' :: acc) cs)
    -- Smart punctuation: dash runs, ellipsis, and orientation-aware
    -- curly quotes. Order matters — longer runs match first so `---`
    -- becomes an em-dash, not en-dash + literal `-`.
    '-' => case cs of
      ('-' :: '-' :: rest) =>
        flushAcc acc ++ [InlSmart EmDash]
          ++ assert_total (parseInlinesAcc [] rest)
      ('-' :: rest) =>
        flushAcc acc ++ [InlSmart EnDash]
          ++ assert_total (parseInlinesAcc [] rest)
      _ => assert_total (parseInlinesAcc ('-' :: acc) cs)
    '.' => case cs of
      ('.' :: '.' :: rest) =>
        flushAcc acc ++ [InlSmart Ellipsis]
          ++ assert_total (parseInlinesAcc [] rest)
      _ => assert_total (parseInlinesAcc ('.' :: acc) cs)
    '"' =>
      let sp = if isOpenContext acc then LDQuote else RDQuote
       in flushAcc acc ++ [InlSmart sp]
            ++ assert_total (parseInlinesAcc [] cs)
    '\'' =>
      let sp = if isOpenContext acc then LSQuote else RSQuote
       in flushAcc acc ++ [InlSmart sp]
            ++ assert_total (parseInlinesAcc [] cs)
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

||| `True` iff `s` ends with a single backslash. Djot uses a trailing
||| backslash on a paragraph line as the hard-break marker — the line
||| break in the rendered output is a real `<br>`, not a soft break that
||| can be collapsed. Only meaningful between lines of the *same*
||| paragraph; at end-of-paragraph the backslash is left literal.
endsWithBackslash : String -> Bool
endsWithBackslash s = case unsnoc (unpack s) of
  Just (_, '\\') => True
  _              => False
  where
    unsnoc : List Char -> Maybe (List Char, Char)
    unsnoc []        = Nothing
    unsnoc [x]       = Just ([], x)
    unsnoc (x :: xs) = case unsnoc xs of
      Just (init, last) => Just (x :: init, last)
      Nothing           => Nothing

||| Drop the trailing character (assumed `\\` per `endsWithBackslash`).
||| Returns the line without its terminating backslash. If the input is
||| empty, returns empty.
dropTrailingChar : String -> String
dropTrailingChar s = case reverse (unpack s) of
  []        => ""
  (_ :: rs) => pack (reverse rs)

||| Parse a paragraph body: consecutive non-blank lines joined by
||| `InlSoftBreak`, OR by `InlHardBreak` when the preceding line ends
||| with a `\\` (Djot's hard-break marker — the `\\` is stripped from
||| the line content before parsing).
|||
||| End-of-paragraph: the last line's trailing `\\` is left literal
||| (Djot only treats the marker as a hard break when followed by
||| another line in the same paragraph).
parseParagraphLines : List1 String -> List Inline
parseParagraphLines (l ::: ls) = go l ls
  where
    go : (cur : String) -> (rest : List String) -> List Inline
    go cur []              = parseInlineLine cur
    go cur (next :: more)  =
      if endsWithBackslash cur
        then parseInlineLine (dropTrailingChar cur)
              ++ (InlHardBreak :: go next more)
        else parseInlineLine cur
              ++ (InlSoftBreak :: go next more)

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
  ||| A run of pipe-table lines, carried verbatim for the table block
  ||| parser. Each line is a row source; the cell parse + alignment
  ||| classification happens in `tableGroupToBlock`.
  TableGroup  : List1 String        -> LineGroup
  ||| A definition-list run: `: term` openers plus their indented
  ||| (and blank-line-bridged) continuation lines. Carried verbatim so
  ||| the def-list parser can split items, identify terms, and parse
  ||| each item's body as nested blocks.
  DefListGroup : List1 String       -> LineGroup

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

||| `True` iff `s` is a plausible pipe-table row: trimmed line begins
||| with `|` and contains at least one further `|` (so it has at least
||| one cell). The trailing `|` is optional but conventional; the cell
||| splitter handles either form.
isTableLine : String -> Bool
isTableLine s = case unpack (trim s) of
  ('|' :: rest) => any (== '|') rest
  _             => False

||| `True` iff `s` is a definition-list opener: `: ` (colon-space) or
||| `:` alone (column-0; no leading indent). Lines that merely begin
||| with `:` but lack the trailing space (like `:emoji:` symbols inside
||| a paragraph) do not open a def list.
isDefListOpener : String -> Bool
isDefListOpener s = case unpack s of
  (':' :: ' ' :: _) => True
  [':']             => True
  _                 => False

||| `True` iff `s` begins with at least one space (a continuation line
||| for an open def-list item).
isIndentedLine : String -> Bool
isIndentedLine s = case unpack s of
  (' ' :: _) => True
  _          => False

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

    -- Greedily collect def-list continuation lines from `xs`.
    -- Returns `(continuation, rest-after-group)`. A trailing run of
    -- blanks at the end of the file is consumed (they are stripped
    -- on the way out by the normal blank-line drop path); a blank line
    -- followed by a non-indented non-opener line ends the group.
    collectDefList : List String -> (List String, List String)
    collectDefList []        = ([], [])
    collectDefList (l :: ls) =
      if isBlankLine l
        then case ls of
          [] => ([], [])
          (l' :: _) =>
            if isIndentedLine l' || isDefListOpener l'
              then let (more, rest) = collectDefList ls
                    in (l :: more, rest)
              else ([], l :: ls)
        else if isIndentedLine l || isDefListOpener l
          then let (more, rest) = collectDefList ls
                in (l :: more, rest)
          else ([], l :: ls)

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
              else if isTableLine x
                then
                  let (rows, rest) = spanList isTableLine (x :: xs)
                      acc'         = flushNormal (reverse cur) acc
                      table        = case rows of
                        []         => acc'  -- impossible (x satisfies)
                        (r :: rs)  =>
                          TableGroup (r ::: rs) :: acc'
                   in assert_total (go [] table rest)
                else if isDefListOpener x
                  then
                    -- Def-list runs span blank lines (a loose item's
                    -- body is a paragraph at indent ≥ 2 separated from
                    -- the term line by a blank). Collect all immediate
                    -- continuation lines (blank, indented, or another
                    -- `:` opener); stop on the first non-blank,
                    -- non-indented, non-opener line.
                    let (rest, rs) = collectDefList xs
                        acc'       = flushNormal (reverse cur) acc
                        defList    = DefListGroup (x ::: rest)
                     in assert_total (go [] (defList :: acc') rs)
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
-- markers, ordered lists with `<n>. ` decimal markers, and task-list
-- items (`- [ ]`/`- [x]` and `+`/`*` variants). Nested lists,
-- continuation-line indentation, and loose task lists arrive in later
-- slices; definition lists land via `DefListGroup` (a separate
-- LineGroup variant) since their multi-line structure spans the blank
-- lines `groupLines` would otherwise split on.
--------------------------------------------------------------------------------

||| Recognise a task-list checkbox at the start of `cs`. Returns
||| `Just (checked, body)` for `[ ] <body>` (unchecked) / `[x] <body>` /
||| `[X] <body>` (checked); `Nothing` otherwise.
parseTaskMarker : List Char -> Maybe (Bool, String)
parseTaskMarker ('[' :: ' ' :: ']' :: ' ' :: rest) = Just (False, pack rest)
parseTaskMarker ('[' :: 'x' :: ']' :: ' ' :: rest) = Just (True,  pack rest)
parseTaskMarker ('[' :: 'X' :: ']' :: ' ' :: rest) = Just (True,  pack rest)
parseTaskMarker _                                  = Nothing

||| Recognise a list-item line. Returns `(style, body, checked)` where
||| `body` is the inline content (everything after the marker — and the
||| task-checkbox token, when present) and `checked` is `Just bool`
||| only for `TaskList` items.
isListLine : String -> Maybe (ListStyle, String, Maybe Bool)
isListLine s = case unpack s of
  ('-' :: ' ' :: rest) => Just (taskOr UnorderedDash     rest)
  ('*' :: ' ' :: rest) => Just (taskOr UnorderedAsterisk rest)
  ('+' :: ' ' :: rest) => Just (taskOr UnorderedPlus     rest)
  cs                   => case parseOrderedMarker cs of
    Just (digits, body) => Just (OrderedDecimal, pack body, Nothing)
    Nothing             => Nothing
  where
    -- If `rest` opens with a task-list checkbox, the item becomes a
    -- TaskList item (the bullet flavour collapses — TaskList is its
    -- own ListStyle per `Cribrum.Djot.Surface`). Otherwise the bullet
    -- flavour is kept and the item is a regular unordered item.
    taskOr : ListStyle -> List Char -> (ListStyle, String, Maybe Bool)
    taskOr fallback rest = case parseTaskMarker rest of
      Just (checked, body) => (TaskList, body,     Just checked)
      Nothing              => (fallback, pack rest, Nothing)

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
  Just (style, body, checked) =>
    Just (style, MkLI emptyAttrs checked Nothing
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

||| Find the first `]` in `xs` and return `(before, after-the-bracket)`,
||| or `Nothing` if no `]` exists. Used to extract a reference label
||| from a `[label]: ...` line.
findCloseRBracket : List Char -> Maybe (List Char, List Char)
findCloseRBracket []           = Nothing
findCloseRBracket (']' :: xs)  = Just ([], xs)
findCloseRBracket (x   :: xs)  = case findCloseRBracket xs of
  Just (ins, rest) => Just (x :: ins, rest)
  Nothing          => Nothing

||| Find the first occurrence of `c` in `xs`, returning the prefix
||| before it and the suffix after it (`c` itself dropped).
breakOnChar : Char -> List Char -> Maybe (List Char, List Char)
breakOnChar _ []        = Nothing
breakOnChar c (x :: xs) =
  if x == c
    then Just ([], xs)
    else case breakOnChar c xs of
      Just (a, b) => Just (x :: a, b)
      Nothing     => Nothing

||| If `s` looks like `<url> "title"`, return `(url, title)` with the
||| surrounding double-quotes stripped. Otherwise `Nothing` (body is
||| just a URL with no title).
extractRefTitle : String -> Maybe (String, String)
extractRefTitle s = case unpack s of
  [] => Nothing
  cs => case reverse cs of
    ('"' :: rcs) => case breakOnChar '"' rcs of
      Just (titleRev, afterOpen) =>
        let url   = trim (pack (reverse afterOpen))
            title = pack (reverse titleRev)
         in if url == ""
              then Nothing
              else Just (url, title)
      Nothing => Nothing
    _ => Nothing

||| Recognise a Djot reference definition line: `[ref]: url` (optionally
||| with a trailing `"title"`). Returns `(label, url, title)` on a
||| match. The label is the bracketed identifier with surrounding
||| whitespace trimmed; the url is everything between `:` and the
||| optional title, trimmed. The title slice handles only the
||| double-quoted form for now; single-quote and paren forms arrive
||| with the parser remainder.
parseRefDef : String -> Maybe (String, String, Maybe String)
parseRefDef src = case unpack src of
  ('[' :: rest) => case findCloseRBracket rest of
    Just (label, afterClose) => case afterClose of
      (':' :: ' ' :: body) =>
        let bodyStr = trim (pack body)
         in case extractRefTitle bodyStr of
              Just (url, title) =>
                if pack label == "" || url == ""
                  then Nothing
                  else Just (pack label, url, Just title)
              Nothing =>
                if pack label == "" || bodyStr == ""
                  then Nothing
                  else Just (pack label, bodyStr, Nothing)
      _ => Nothing
    Nothing => Nothing
  _ => Nothing

||| Convert one NORMAL line group into a block.
|||
||| Order matters: thematic break is checked first (a single `---` line is
||| not a heading and not a paragraph). Heading next; then reference
||| definition (single-line `[ref]: url`); then list block; everything
||| else falls through to paragraph.
normalGroupToBlock : List1 String -> Block
normalGroupToBlock (l ::: ls) =
  if isNil ls && isThematicBreak l
    then ThematicBreak emptyAttrs
    else case parseHeadingMarker l of
           Just (lvl, rest) =>
             if isNil ls
               then Heading emptyAttrs lvl (parseInlineLine rest)
               else Paragraph emptyAttrs (parseParagraphLines (l ::: ls))
           Nothing =>
             if isNil ls
               then case parseRefDef l of
                 Just (label, url, title) => RefDef label url title
                 Nothing => case tryParseList (l ::: ls) of
                   Just listBlock => listBlock
                   Nothing => Paragraph emptyAttrs
                                (parseParagraphLines (l ::: ls))
               else case tryParseList (l ::: ls) of
                 Just listBlock => listBlock
                 Nothing => Paragraph emptyAttrs
                              (parseParagraphLines (l ::: ls))

--------------------------------------------------------------------------------
-- Pipe tables (slice).
--------------------------------------------------------------------------------

||| Split a table-row source into trimmed cell strings. Drops the
||| optional leading and trailing `|`, splits the rest on `|`, and
||| trims surrounding whitespace from each cell.
splitTableRow : String -> List String
splitTableRow line =
  let chars  = unpack (trim line)
      inner  = case chars of
                 ('|' :: rest) => rest
                 _             => chars
      inner' = case reverse inner of
                 ('|' :: rs) => reverse rs
                 _           => inner
   in map (trim . pack) (splitOn '|' inner')
  where
    splitOn : Char -> List Char -> List (List Char)
    splitOn _ []        = [[]]
    splitOn c (x :: xs) = case splitOn c xs of
      (cur :: rest) =>
        if x == c then [] :: cur :: rest
                  else (x :: cur) :: rest
      []            => [[x]]  -- impossible: splitOn always returns ≥ 1

||| Classify one alignment-row cell. A cell is an alignment marker
||| iff it consists of a leading optional `:`, three-or-more `-`, and
||| a trailing optional `:` (no other characters). Returns the
||| corresponding `Align`, or `Nothing` if the cell isn't a marker.
parseAlignCell : String -> Maybe Align
parseAlignCell s =
  let chars = unpack (trim s)
      (left, mid)  = case chars of
        (':' :: r) => (True, r)
        _          => (False, chars)
      revMid       = reverse mid
      (right, bar) = case revMid of
        (':' :: r) => (True, reverse r)
        _          => (False, mid)
   in if length bar >= 3 && all (== '-') bar
        then Just $ case (left, right) of
          (True,  True)  => AlignCenter
          (True,  False) => AlignLeft
          (False, True)  => AlignRight
          (False, False) => AlignNone
        else Nothing

||| `True` iff every cell on the row is an alignment marker AND there
||| is at least one cell. The cell count is the column count of the
||| surrounding table.
parseAlignRow : String -> Maybe (List Align)
parseAlignRow s = case splitTableRow s of
  [] => Nothing
  cs => traverse parseAlignCell cs

||| Build a `TableCell` from a raw cell source + an alignment.
makeCell : Align -> String -> TableCell
makeCell a body = MkCell a (parseInlineLine body)

||| Build a `TableRow` from a raw row source. If `aligns` is shorter
||| than the cells (or empty), missing positions get `AlignNone`.
makeRow : (isHeader : Bool) -> List Align -> String -> TableRow
makeRow header aligns line =
  let cells = splitTableRow line
   in MkRow header (zipWithAlign aligns cells)
  where
    -- Pair cells with their per-column align, padding the alignment
    -- list with AlignNone if the row has more cells than the
    -- alignment row promised.
    zipWithAlign : List Align -> List String -> List TableCell
    zipWithAlign _        []        = []
    zipWithAlign []       (c :: cs) =
      makeCell AlignNone c :: zipWithAlign [] cs
    zipWithAlign (a :: as) (c :: cs) =
      makeCell a c :: zipWithAlign as cs

--------------------------------------------------------------------------------
-- Definition-list group processing.
--------------------------------------------------------------------------------

||| Strip up to `n` leading space characters from `s`. Tabs are not
||| expanded; lines indented with `\t` are passed through unchanged.
dropLeadingSpaces : Nat -> String -> String
dropLeadingSpaces n s = pack (drop' n (unpack s))
  where
    drop' : Nat -> List Char -> List Char
    drop' Z     xs               = xs
    drop' (S _) []               = []
    drop' (S k) (' ' :: xs)      = drop' k xs
    drop' (S _) xs               = xs

||| Strip a def-list opener prefix from a line. `: foo` -> `foo`,
||| `:` -> `""`, anything else -> the input verbatim.
stripDefOpener : String -> String
stripDefOpener s = case unpack s of
  (':' :: ' ' :: rest) => pack rest
  [':']                => ""
  _                    => s

||| Group raw def-list lines into per-item runs. Each run starts with
||| an opener line (kept as the run's first element); blanks and
||| continuation lines stay attached to the preceding opener.
splitDefItems : List String -> List (List String)
splitDefItems []        = []
splitDefItems (l :: ls) = go [l] [] ls
  where
    -- `cur` is the current item's lines in reverse order; `acc` is the
    -- accumulated items in reverse order. Each `: ` line starts a new
    -- run.
    go : (cur : List String) -> (acc : List (List String))
       -> List String -> List (List String)
    go cur acc []        = reverse (reverse cur :: acc)
    go cur acc (x :: xs) =
      if isDefListOpener x
        then go [x] (reverse cur :: acc) xs
        else go (x :: cur) acc xs

||| Split a per-item run into `(termLines, bodyLines)`. The term is the
||| first paragraph: the opener line (with `: ` stripped) plus any
||| immediately-following non-blank continuation lines. The body is
||| everything after the first blank, with the dedent (up to 2 spaces)
||| applied so the inner block parser can re-grouping the body
||| recursively.
splitTermBody : List String -> (List String, List String)
splitTermBody [] = ([], [])
splitTermBody (opener :: more) =
  let termHead     = stripDefOpener opener
      (cont, rest) = spanList (not . isBlankLine) more
      bodyRaw      = dropWhile isBlankLine rest
      body         = map (dropLeadingSpaces 2) bodyRaw
   in (termHead :: cont, body)

||| Convert a TableGroup's raw lines into a `Table` block.
|||
||| If the second row is an alignment row, the first row becomes the
||| header (with the cell alignments applied) and rows from the third
||| onward are body rows. Otherwise no alignment is in effect, no
||| header is declared, and every row is a body row with `AlignNone`
||| cells.
tableGroupToBlock : List1 String -> Block
tableGroupToBlock (l ::: ls) = case ls of
  (alignLine :: rest) => case parseAlignRow alignLine of
    Just aligns =>
      let header = makeRow True  aligns l
          body   = map (makeRow False aligns) rest
       in Table emptyAttrs Nothing (header :: body)
    Nothing =>
      let rows = map (makeRow False []) (l :: ls)
       in Table emptyAttrs Nothing rows
  [] =>
    let row = makeRow False [] l
     in Table emptyAttrs Nothing [row]

mutual
  ||| Build a `Definition`-style `ListBlock` from a `DefListGroup`'s
  ||| raw lines. Each per-item run becomes a `ListItem` whose `term` is
  ||| the inline parse of the term paragraph and whose `content` is
  ||| the body parsed recursively as blocks.
  defListGroupToBlock : List1 String -> Block
  defListGroupToBlock (l ::: ls) =
    let items = map mkItem (splitDefItems (l :: ls))
     in ListBlock emptyAttrs Definition Nothing False items
    where
      mkItem : List String -> ListItem
      mkItem run =
        let (termLs, bodyLs) = splitTermBody run
            termSoft         = parseTermLines termLs
            bodyBlocks       = parseBodyBlocks bodyLs
         in MkLI emptyAttrs Nothing (Just termSoft) bodyBlocks

  ||| Inline-parse the term paragraph: each line through
  ||| `parseInlineLine`, joined by `InlSoftBreak`.
  parseTermLines : List String -> List Inline
  parseTermLines []        = []
  parseTermLines (l :: ls) = case ls of
    [] => parseInlineLine l
    _  => parseInlineLine l ++ (InlSoftBreak :: parseTermLines ls)

  ||| Parse the dedented body lines as a sequence of blocks by re-
  ||| running the block grouper + group→block conversion.
  parseBodyBlocks : List String -> List Block
  parseBodyBlocks ls =
    assert_total (map groupToBlock (groupLines ls))

  ||| Convert a LineGroup into a Block. Quote groups recurse.
  public export
  groupToBlock : LineGroup -> Block
  groupToBlock (NormalGroup g)    = normalGroupToBlock g
  groupToBlock (QuoteGroup  gs)   =
    BlockQuote emptyAttrs (assert_total (map groupToBlock gs))
  groupToBlock (CodeGroup info b) =
    CodeBlock emptyAttrs info b
  groupToBlock (TableGroup rows)  = tableGroupToBlock rows
  groupToBlock (DefListGroup rs)  = defListGroupToBlock rs

-- ============================================================
-- Reference-link resolution (two-pass).
--
-- Phase 1: parse into raw blocks (with LinkReference label for any
--          [text][label] / [text][] form).
-- Phase 2: walk the block list collecting RefDef labels -> urls,
--          then walk the inline trees rewriting LinkReference label
--          to LinkInline url when label is defined. Undefined refs
--          stay as LinkReference (the elaborator emits them as
--          anchor-href placeholders so the failure is visible).
-- ============================================================

mutual
  ||| Build a lookup table from a list of blocks. Only top-level
  ||| RefDef blocks contribute; nested defs (inside block quotes) are
  ||| recursively scanned.
  collectRefDefs : List Block -> List (String, String)
  collectRefDefs []        = []
  collectRefDefs (b :: bs) =
    refDefsFromBlock b ++ assert_total (collectRefDefs bs)

  refDefsFromBlock : Block -> List (String, String)
  refDefsFromBlock (RefDef l u _)      = [(l, u)]
  refDefsFromBlock (BlockQuote _ bs')  =
    assert_total (collectRefDefs bs')
  refDefsFromBlock _                   = []

mutual
  ||| Resolve `LinkReference label` to `LinkInline url` when the label
  ||| is known. Other forms (LinkInline, LinkAuto) pass through.
  resolveInline : List (String, String) -> Inline -> Inline
  resolveInline tab i = case i of
    InlLink a (LinkReference l) xs => case lookup l tab of
      Just url => InlLink a (LinkInline url Nothing)
                    (assert_total (resolveInlines tab xs))
      Nothing  => InlLink a (LinkReference l)
                    (assert_total (resolveInlines tab xs))
    InlLink a r xs   =>
      InlLink a r (assert_total (resolveInlines tab xs))
    InlImage a r xs  =>
      InlImage a r (assert_total (resolveInlines tab xs))
    InlEmph xs       =>
      InlEmph       (assert_total (resolveInlines tab xs))
    InlStrong xs     =>
      InlStrong     (assert_total (resolveInlines tab xs))
    InlHighlight xs  =>
      InlHighlight  (assert_total (resolveInlines tab xs))
    InlSuper xs      =>
      InlSuper      (assert_total (resolveInlines tab xs))
    InlSub xs        =>
      InlSub        (assert_total (resolveInlines tab xs))
    InlInsert xs     =>
      InlInsert     (assert_total (resolveInlines tab xs))
    InlDelete xs     =>
      InlDelete     (assert_total (resolveInlines tab xs))
    InlSpan a xs     =>
      InlSpan a     (assert_total (resolveInlines tab xs))
    other            => other

  resolveInlines : List (String, String) -> List Inline -> List Inline
  resolveInlines tab = map (resolveInline tab)

mutual
  resolveBlock : List (String, String) -> Block -> Block
  resolveBlock tab b = case b of
    Paragraph a is        => Paragraph a (resolveInlines tab is)
    Heading a lvl is      => Heading a lvl (resolveInlines tab is)
    BlockQuote a bs       =>
      BlockQuote a (assert_total (resolveBlocks tab bs))
    ListBlock a s st t is =>
      ListBlock a s st t (assert_total (map (resolveItem tab) is))
    Div a bs              =>
      Div a (assert_total (resolveBlocks tab bs))
    Table a cap rs        =>
      Table a (map (resolveInlines tab) cap)
            (map (resolveRow tab) rs)
    FootnoteDef a l bs    =>
      FootnoteDef a l (assert_total (resolveBlocks tab bs))
    other                 => other

  resolveBlocks : List (String, String) -> List Block -> List Block
  resolveBlocks tab = map (resolveBlock tab)

  resolveItem : List (String, String) -> ListItem -> ListItem
  resolveItem tab (MkLI a c t bs) =
    MkLI a c (map (resolveInlines tab) t)
      (assert_total (resolveBlocks tab bs))

  resolveRow : List (String, String) -> TableRow -> TableRow
  resolveRow tab (MkRow h cs) = MkRow h (map (resolveCell tab) cs)

  resolveCell : List (String, String) -> TableCell -> TableCell
  resolveCell tab (MkCell a is) = MkCell a (resolveInlines tab is)

||| Parse a Djot document, then resolve reference-link forms against
||| the document's RefDef blocks. Total; never fails on the current
||| slice's input classes. Returns `Either` so future constructs can
||| produce located errors without changing the signature.
public export
parseDoc : String -> Either ParseError Doc
parseDoc src =
  let ls    = lines src
      raw   = map groupToBlock (groupLines ls)
      table = collectRefDefs raw
      resolved = resolveBlocks table raw
   in Right (MkDoc resolved)
