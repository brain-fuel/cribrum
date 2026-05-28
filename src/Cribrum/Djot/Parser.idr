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

||| Scan `cs` for the first occurrence of the two-character closer
||| `c1 c2` (in order). Returns body before the closer and the rest
||| after it. Used by `{+...+}` / `{-...-}` / `{=...=}` spans.
findClose2 : Char -> Char -> List Char -> Maybe (List Char, List Char)
findClose2 _  _  []                = Nothing
findClose2 c1 c2 (x :: y :: rest)  =
  if x == c1 && y == c2
    then Just ([], rest)
    else case findClose2 c1 c2 (y :: rest) of
      Just (ins, after) => Just (x :: ins, after)
      Nothing           => Nothing
findClose2 _  _  _                 = Nothing

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

||| Peel a Djot hard-break marker off the reversed plain-text accumulator.
||| The marker is a literal `\\` optionally followed (in source order) by
||| trailing whitespace — i.e. in the reversed accumulator the pattern is
||| `(' '|'\t')* '\\' rest`. Returns `Just rest` (accumulator with the
||| marker stripped) when the pattern matches, else `Nothing`.
stripHardBreakMarker : List Char -> Maybe (List Char)
stripHardBreakMarker []           = Nothing
stripHardBreakMarker ('\\' :: rs) = Just rs
stripHardBreakMarker (c :: rs)    =
  if c == ' ' || c == '\t' then stripHardBreakMarker rs else Nothing

||| Emphasis opener (`*`/`_`) is blocked when the next character is
||| whitespace or end-of-input. `cs` is the character list after the
||| marker.
openerBlocked : List Char -> Bool
openerBlocked []       = True
openerBlocked (c :: _) = isSpace c

||| Emphasis closer (`*`/`_`) is blocked when the character immediately
||| before it (the last character of `inner`) is whitespace. `inner` is
||| the body between opener and closer.
closerBlocked : List Char -> Bool
closerBlocked inner = case reverse inner of
  []        => True
  (c :: _)  => isSpace c

--------------------------------------------------------------------------------
-- Inline verbatim helpers.
--
-- Djot spec (`<doc/syntax.html>` §Verbatim): an inline verbatim span
-- opens with N backticks (any N ≥ 1) and closes with the next run of
-- EXACTLY N backticks. Runs of any other length inside the span are
-- literal. If the body both begins and ends with a space (but is not
-- entirely whitespace), one space is stripped from each end so authors
-- can write `` ` ``a`` ` `` to produce ``a`` inside <code>.
-- An unclosed opener consumes the rest of the inline content.
--------------------------------------------------------------------------------

||| Walk a leading run of backticks in `cs`, returning `(run, rest)`.
takeBacktickRun : List Char -> (List Char, List Char)
takeBacktickRun ('`' :: cs) =
  let (more, rest) = takeBacktickRun cs in ('`' :: more, rest)
takeBacktickRun xs          = ([], xs)

||| Locate a closing run of EXACTLY `n` backticks. Returns the body
||| before the closer and the input after it. `Nothing` means no
||| matching closer exists in the input (caller treats as
||| "run-to-end-of-inline").
findVerbatimClose : (n : Nat) -> List Char -> Maybe (List Char, List Char)
findVerbatimClose n = go []
  where
    go : List Char -> List Char -> Maybe (List Char, List Char)
    go _   []                = Nothing
    go acc xs@('`' :: _)     =
      let (ticks, after) = takeBacktickRun xs in
      if length ticks == n
        then Just (reverse acc, after)
        else assert_total (go (reverse ticks ++ acc) after)
    go acc (c :: cs)         = assert_total (go (c :: acc) cs)

||| `True` iff `c` is ASCII punctuation per Djot (`!"#$%&'()*+,-./:;<=>?@[\]^_\`{|}~`).
||| Djot backslash escapes consume punctuation; non-punctuation chars
||| keep the backslash literal.
isAsciiPunct : Char -> Bool
isAsciiPunct c =
     c == '!' || c == '"' || c == '#' || c == '$' || c == '%'
  || c == '&' || c == '\'' || c == '(' || c == ')' || c == '*'
  || c == '+' || c == ',' || c == '-' || c == '.' || c == '/'
  || c == ':' || c == ';' || c == '<' || c == '=' || c == '>'
  || c == '?' || c == '@' || c == '[' || c == '\\' || c == ']'
  || c == '^' || c == '_' || c == '`' || c == '{' || c == '|'
  || c == '}' || c == '~'

||| Strip one leading + one trailing space from `body` iff the body
||| both begins and ends with a space AND is not entirely whitespace.
verbatimStrip : List Char -> List Char
verbatimStrip body = case body of
  (' ' :: _) => case reverse body of
    (' ' :: _) =>
      if all (== ' ') body
        then body
        else case body of
          (_ :: rest) => reverse (drop 1 (reverse rest))
          []          => body
    _          => body
  _          => body

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
    -- Emphasis / strong flanking rule (Djot): the marker is emphasised
    -- iff the opener and closer agree on their adjacent-whitespace
    -- status. Both have inside-whitespace (`_ a _`) → emphasis; both
    -- have non-whitespace inside (`_a_`) → emphasis; asymmetric
    -- (`_ a_` or `_a _`) → marker stays literal. Empty body always
    -- fails. On any rule failure the marker joins the plain-text
    -- accumulator and parsing continues.
    '_' => case findClose '_' cs of
      Just (inner, after) =>
        if inner == [] || openerBlocked cs /= closerBlocked inner
          then assert_total (parseInlinesAcc ('_' :: acc) cs)
          else flushAcc acc
            ++ [InlEmph (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('_' :: acc) cs)
    '*' => case findClose '*' cs of
      Just (inner, after) =>
        if inner == [] || openerBlocked cs /= closerBlocked inner
          then assert_total (parseInlinesAcc ('*' :: acc) cs)
          else flushAcc acc
            ++ [InlStrong (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('*' :: acc) cs)
    -- Djot decorator spans: `{+ … +}` (insert), `{- … -}` (delete),
    -- `{= … =}` (highlight / mark). Each takes inline content between
    -- the open and close markers and elaborates to the matching HTML
    -- element. Empty body falls back to literal `{x`.
    '{' => case cs of
      ('+' :: rest) => case findClose2 '+' '}' rest of
        Just (inner, after) =>
          if inner == []
            then assert_total (parseInlinesAcc ('{' :: acc) cs)
            else flushAcc acc
              ++ [InlInsert (assert_total (parseInlines inner))]
              ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('{' :: acc) cs)
      ('-' :: rest) => case findClose2 '-' '}' rest of
        Just (inner, after) =>
          if inner == []
            then assert_total (parseInlinesAcc ('{' :: acc) cs)
            else flushAcc acc
              ++ [InlDelete (assert_total (parseInlines inner))]
              ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('{' :: acc) cs)
      ('=' :: rest) => case findClose2 '=' '}' rest of
        Just (inner, after) =>
          if inner == []
            then assert_total (parseInlinesAcc ('{' :: acc) cs)
            else flushAcc acc
              ++ [InlHighlight (assert_total (parseInlines inner))]
              ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('{' :: acc) cs)
      _ => assert_total (parseInlinesAcc ('{' :: acc) cs)
    -- Djot backslash escape: `\<punct>` -> literal punct (drop the `\`).
    -- `\<non-punct>` keeps both characters literal. A trailing `\` with
    -- nothing after stays literal too.
    '\\' => case cs of
      (n :: rest) =>
        if isAsciiPunct n
          then assert_total (parseInlinesAcc (n :: acc) rest)
          else assert_total (parseInlinesAcc ('\\' :: acc) cs)
      []          => assert_total (parseInlinesAcc ('\\' :: acc) cs)
    '`' =>
      let (more, afterOpen) = takeBacktickRun cs
          openerLen         = S (length more)
       in case findVerbatimClose openerLen afterOpen of
            Just (inner, after) =>
              if inner == []
                then assert_total (parseInlinesAcc ('`' :: acc) cs)
                else flushAcc acc
                  ++ [InlVerbatim emptyAttrs (pack (verbatimStrip inner))]
                  ++ assert_total (parseInlinesAcc [] after)
            Nothing =>
              -- Unclosed opener: per spec, consumes the rest of the
              -- inline content. (The inline parser runs per line, so
              -- "rest" here is line-bounded; multi-line verbatim still
              -- requires the paragraph-spanning lift.)
              if afterOpen == []
                then assert_total (parseInlinesAcc ('`' :: acc) cs)
                else flushAcc acc
                  ++ [InlVerbatim emptyAttrs (pack (verbatimStrip afterOpen))]
    '[' => case cs of
      -- Footnote reference `[^label]` — `^` immediately after `[` and a
      -- non-empty label terminated by `]`. Falls back to literal `[` if
      -- the label is empty or no `]` is found.
      ('^' :: rest) => case findClose ']' rest of
        Just (label, after) =>
          if label == []
            then assert_total (parseInlinesAcc ('[' :: acc) cs)
            else flushAcc acc
                   ++ [InlFootnoteRef (pack label)]
                   ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('[' :: acc) cs)
      _ => case parseLinkBody cs of
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
    -- Line break inside a paragraph body. The paragraph driver joins
    -- continuation lines with literal '\n' so multi-line constructs
    -- (verbatim spans) can swallow the newline naturally; outside such
    -- constructs the newline emits a soft/hard break inline. A trailing
    -- `\\` (with optional whitespace after) flips the break to hard.
    '\n' => case stripHardBreakMarker acc of
      Just acc' => flushAcc acc' ++ [InlHardBreak]
        ++ assert_total (parseInlinesAcc [] cs)
      Nothing   => flushAcc acc ++ [InlSoftBreak]
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

||| Parse a paragraph body: consecutive non-blank lines joined by
||| `InlSoftBreak`, OR by `InlHardBreak` when the preceding line ends
||| with a `\\` (Djot's hard-break marker — the `\\` is stripped from
||| the line content before parsing).
|||
||| End-of-paragraph: the last line's trailing `\\` is left literal
||| (Djot only treats the marker as a hard break when followed by
||| another line in the same paragraph).
parseParagraphLines : List1 String -> List Inline
parseParagraphLines (l ::: ls) = parseInlines (joinPara (l :: ls))
  where
    -- Join paragraph lines with literal '\n' so the inline tokenizer
    -- sees the entire body in one pass. The tokenizer's '\n' handler
    -- emits InlSoftBreak (or InlHardBreak if the preceding text ends
    -- with a `\\` per Djot). Verbatim spans naturally consume newlines
    -- by virtue of `findVerbatimClose` walking the full char list.
    joinPara : List String -> List Char
    joinPara []        = []
    joinPara [s]       = unpack s
    joinPara (s :: ss) = unpack s ++ ('\n' :: joinPara ss)

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
  ||| A footnote definition: `[^label]:` opener plus indented body lines
  ||| (continuations can span blank lines, like def-lists). The label
  ||| is parsed out at group time; `body` is the rest of the opener
  ||| line + dedented continuation lines, ready for recursive block
  ||| parsing.
  FootnoteGroup : (label : String) -> (body : List String) -> LineGroup
  ||| A block-level attribute prefix line `{...}` that attaches its
  ||| parsed Attrs to the next non-attribute block. Multiple
  ||| consecutive attribute lines stack via `mergeAttrs`.
  AttrPrefixGroup : Attrs -> LineGroup
  ||| A fenced div (`::: cls`/`::: cls1 cls2`/bare `:::`) with its
  ||| recursively-grouped interior. Classes harvested at open time
  ||| land in `attrs`; the body is everything between opener and the
  ||| matching close (or EOF if unclosed), regrouped via `groupLines`.
  DivGroup    : Attrs -> List LineGroup -> LineGroup

||| `True` iff the line starts with `>` followed by space, OR is exactly `>`
||| (an empty quote line — Djot allows this).
isQuotePrefixed : String -> Bool
isQuotePrefixed s = case unpack s of
  ('>' :: ' ' :: _) => True
  ['>']             => True
  _                 => False

||| Count leading occurrences of `ch` in a string.
countFenceChars : Char -> String -> Nat
countFenceChars ch = go 0 . unpack
  where
    go : Nat -> List Char -> Nat
    go n (c :: cs) = if c == ch then go (S n) cs else n
    go n _         = n

||| Legacy alias retained so existing callers keep working — Djot's
||| backtick fence count.
countBackticks : String -> Nat
countBackticks = countFenceChars '`'

||| If `s` is a fenced code-block opening line — 3+ of the same fence
||| character (`` ` `` or `~`) optionally followed by an info string —
||| return `Just (fenceChar, fenceLen, info)`. Otherwise `Nothing`.
|||
||| Djot spec: opener is 3+ of either backtick or tilde; the info
||| string is the rest of the line, trimmed. For backtick fences the
||| info string must NOT contain further backticks (so an inline
||| verbatim run on the same line — `` ``` x ``` `` — doesn't open a
||| code block). Tilde fences impose no such restriction. Up to 3
||| leading spaces of indentation are tolerated on the opener.
parseCodeFenceOpen : String -> Maybe (Char, Nat, String)
parseCodeFenceOpen s =
  let stripped = pack (dropLeadingSpaceN 3 (unpack s))
      tickN    = countFenceChars '`' stripped
      tildeN   = countFenceChars '~' stripped
   in if tickN >= 3
        then let after = pack (drop tickN (unpack stripped)) in
             if not (any (== '`') (unpack after))
               then Just ('`', tickN, trim after)
               else Nothing
        else if tildeN >= 3
          then let after = pack (drop tildeN (unpack stripped)) in
               Just ('~', tildeN, trim after)
          else Nothing
  where
    dropLeadingSpaceN : Nat -> List Char -> List Char
    dropLeadingSpaceN Z     xs            = xs
    dropLeadingSpaceN (S k) (' ' :: rest) = dropLeadingSpaceN k rest
    dropLeadingSpaceN _     xs            = xs

||| `True` if `s` is a CLOSING fence of length `n` using fence character
||| `ch`: exactly `n` of `ch` (and only whitespace afterwards).
isCodeFenceClose : (ch : Char) -> (n : Nat) -> String -> Bool
isCodeFenceClose ch n s =
  let trimmed = trim s
      bs      = countFenceChars ch trimmed
   in bs == n && length (unpack trimmed) == n

||| If `s` is a fenced-div opening line — 3+ colons optionally followed
||| by class-name token(s) — return `Just (colonCount, attrs)` with
||| classes harvested from the trailing token list. Up to 3 leading
||| spaces of indentation are tolerated on the opener.
|||
||| Djot syntax: `::: cls`, `:::: cls1 cls2`, or bare `:::` (no class).
||| Class tokens are whitespace-separated identifiers; an empty trailing
||| run yields an attrs-less opener.
parseFencedDivOpen : String -> Maybe (Nat, Attrs)
parseFencedDivOpen s =
  let stripped = pack (dropLeadingSpaceN 3 (unpack s))
      n        = countFenceChars ':' stripped
   in if n >= 3
        then let after = pack (drop n (unpack stripped))
                 rest  = trim after
              in if rest == ""
                   then Just (n, emptyAttrs)
                   else
                     let toks = filter (/= "") (words rest)
                         attrs = MkAttrs Nothing toks []
                      in Just (n, attrs)
        else Nothing
  where
    dropLeadingSpaceN : Nat -> List Char -> List Char
    dropLeadingSpaceN Z     xs            = xs
    dropLeadingSpaceN (S k) (' ' :: rest) = dropLeadingSpaceN k rest
    dropLeadingSpaceN _     xs            = xs

||| `True` iff `s` is a fenced-div CLOSING line of length ≥ `n`: a run
||| of at least `n` colons alone on the line (only whitespace after).
isFencedDivClose : (n : Nat) -> String -> Bool
isFencedDivClose n s =
  let trimmed = trim s
      cs      = countFenceChars ':' trimmed
   in cs >= n && length (unpack trimmed) == cs

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

||| `True` iff `s` is a thematic break (3+ matching `-` or `*` runs,
||| optionally separated by whitespace). Hoisted ahead of the
||| blockquote lazy-continuation gate so the gate can disqualify
||| thematic-break lines from lazy continuation.
isThematicBreakLine : String -> Bool
isThematicBreakLine s =
  let trimmed = trim s
   in case unpack trimmed of
        []        => False
        (c :: cs) =>
          (c == '-' || c == '*')
            && let marks = filter (not . isSpace) (c :: cs)
                in length marks >= 3 && all (== c) marks

||| `True` iff `s` begins with at least one space (a continuation line
||| for an open def-list item).
isIndentedLine : String -> Bool
isIndentedLine s = case unpack s of
  (' ' :: _) => True
  _          => False

||| `True` iff `s` starts (after optional whitespace) with the Djot
||| block-comment opener `{%`.
isBlockCommentStart : String -> Bool
isBlockCommentStart s = case unpack (trim s) of
  ('{' :: '%' :: _) => True
  _                 => False

||| `True` iff `s` contains the Djot block-comment closer `%}` at any
||| position. Used to consume the trailing line of a multi-line
||| comment block.
hasBlockCommentEnd : String -> Bool
hasBlockCommentEnd s = go (unpack s)
  where
    go : List Char -> Bool
    go []              = False
    go ('%' :: '}' :: _) = True
    go (_ :: xs)       = go xs

||| Recognise a footnote-definition opener: `[^label]:` optionally
||| followed by a space and inline text. Returns `(label, rest)` where
||| `rest` is the rest of the opener line after the `:` (with one
||| leading space dropped). `Nothing` if not a footnote opener.
parseFootnoteOpener : String -> Maybe (String, String)
parseFootnoteOpener s = case unpack s of
  ('[' :: '^' :: more) => case findCloseRBracket more of
    Just (label, ':' :: rest) =>
      if label == []
        then Nothing
        else
          let restStr = case rest of
                          (' ' :: r) => pack r
                          _          => pack rest
           in Just (pack label, restStr)
    _ => Nothing
  _ => Nothing
  where
    findCloseRBracket : List Char -> Maybe (List Char, List Char)
    findCloseRBracket []           = Nothing
    findCloseRBracket (']' :: xs)  = Just ([], xs)
    findCloseRBracket (x   :: xs)  = case findCloseRBracket xs of
      Just (ins, rest) => Just (x :: ins, rest)
      Nothing          => Nothing

||| `True` iff `s` opens a footnote definition.
isFootnoteOpener : String -> Bool
isFootnoteOpener s = case parseFootnoteOpener s of
  Just _  => True
  Nothing => False

||| Strip up to `n` leading space characters from `s`. Tabs are not
||| expanded; lines indented with `\t` are passed through unchanged.
||| Hoisted ahead of `groupLines` so the footnote branch can use it
||| (def-list parsing further down also references this).
dropLeadingSpaces : Nat -> String -> String
dropLeadingSpaces n s = pack (drop' n (unpack s))
  where
    drop' : Nat -> List Char -> List Char
    drop' Z     xs               = xs
    drop' (S _) []               = []
    drop' (S k) (' ' :: xs)      = drop' k xs
    drop' (S _) xs               = xs

||| Tokenise the body of a `{...}` block into whitespace-separated
||| chunks. A `key="quoted value"` chunk is kept intact (whitespace
||| inside the double-quotes does not split the token). Returns the
||| tokens in source order.
|||
||| The implementation walks char-by-char with a reversed accumulator
||| of the current token. The outer recursion structurally shrinks the
||| input list. The quoted-value branch is gated by `assert_total`
||| because the totality checker can't see that `consumeQuoted` always
||| shrinks the remainder (it either stops at the closing `"` or
||| consumes the entire tail).
splitAttrTokens : List Char -> List String
splitAttrTokens = go []
  where
    consumeQuoted : List Char -> (List Char, List Char)
    consumeQuoted []           = ([], [])
    consumeQuoted ('"' :: rs)  = ([], rs)
    consumeQuoted (c   :: rs)  =
      let (inner, rest) = consumeQuoted rs in (c :: inner, rest)

    go : List Char -> List Char -> List String
    go acc []          = if null acc then [] else [pack (reverse acc)]
    go acc (c :: cs)   =
      if isSpace c
        then if null acc
               then go [] cs
               else pack (reverse acc) :: go [] cs
        else case c of
          '"' =>
            let (q, rest) = consumeQuoted cs
                accQ      = reverse (unpack ("\"" ++ pack q ++ "\""))
                              ++ acc
             in assert_total (go accQ rest)
          _   => go (c :: acc) cs

||| Parse a single attribute token into an Attrs delta. `#x` -> id `x`;
||| `.x` -> class `x`; `key=val` -> pair (val with surrounding `"`
||| stripped). Unrecognised tokens become a (key, "") pair so the
||| pre-pass at least preserves something — but a malformed `{...}`
||| line should never reach this; the recognizer rejects it first.
tokenToAttrs : String -> Attrs
tokenToAttrs s = case unpack s of
  ('#' :: rest) => MkAttrs (Just (pack rest)) [] []
  ('.' :: rest) => MkAttrs Nothing [pack rest] []
  cs            => case break (== '=') cs of
    (key, '=' :: val) =>
      let v = case val of
                ('"' :: r) => case reverse r of
                                ('"' :: rs) => pack (reverse rs)
                                _           => pack val
                _          => pack val
       in MkAttrs Nothing [] [(pack key, v)]
    _ => MkAttrs Nothing [] [(s, "")]

||| Merge two Attrs: later id/key=val wins; classes append.
mergeAttrs : Attrs -> Attrs -> Attrs
mergeAttrs (MkAttrs i1 c1 p1) (MkAttrs i2 c2 p2) =
  let i = case i2 of Just _ => i2; Nothing => i1
      c = c1 ++ c2
      p = p1 ++ p2  -- last wins by virtue of `lookup` returning first;
                    -- but for attribute serialisation the later pair
                    -- should override. We keep both and let the
                    -- elaborator's first-wins lookup find the LAST
                    -- emission by reversing on emit — handled in the
                    -- elaborator.
   in MkAttrs i c p

||| Parse the inside of a `{...}` line into Attrs by splitting on
||| whitespace and merging per-token attrs.
parseAttrBody : String -> Attrs
parseAttrBody body =
  let toks = splitAttrTokens (unpack body)
   in foldl mergeAttrs emptyAttrs (map tokenToAttrs toks)

||| Recognise a block-level attribute prefix line: `{...}` containing
||| only attribute tokens, optionally surrounded by whitespace.
||| Returns the parsed Attrs on success. A line that opens `{` but
||| never closes, or that contains stray content after `}`, is NOT an
||| attribute line (and falls through to paragraph).
|||
||| Decorator spans (`{+ … +}`, `{- … -}`, `{= … =}`) share the brace
||| syntax but are inline content, not block attributes. They are
||| disambiguated by the first non-whitespace character inside the
||| braces: `+`, `-`, `=` rule out the attribute interpretation so the
||| line falls through to the paragraph and the inline parser handles
||| the decorator.
parseAttrBlockLine : String -> Maybe Attrs
parseAttrBlockLine s =
  let chars = unpack (trim s)
   in case chars of
        ('{' :: rest) => case reverse rest of
          ('}' :: revBody) =>
            let bodyChars = reverse revBody
                body      = pack bodyChars
                firstNon  = dropWhile (\c => c == ' ' || c == '\t') bodyChars
             in case firstNon of
                  ('+' :: _) => Nothing
                  ('-' :: _) => Nothing
                  ('=' :: _) => Nothing
                  _          => Just (parseAttrBody body)
          _ => Nothing
        _ => Nothing

||| `True` iff `s` is an attribute-prefix line.
isAttrBlockLine : String -> Bool
isAttrBlockLine s = case parseAttrBlockLine s of
  Just _  => True
  Nothing => False

||| `True` iff `s` is a non-blank line that does NOT open any of the
||| block constructs the grouper recognises — i.e. a plausible
||| paragraph-continuation line. Used by the blockquote lazy-
||| continuation rule: an unprefixed line that satisfies this predicate
||| extends the current blockquote's last paragraph (matches Djot's
||| "lazy continuation" semantics on blockquote-002 / -006 / -007 /
||| -013).
|||
||| List/table/footnote openers are intentionally NOT excluded: the
||| current slice doesn't exercise mid-blockquote list breaks, so the
||| broader exclusion list would over-restrict lazy continuation.
isParagraphContinuable : String -> Bool
isParagraphContinuable s =
  not (s == "" || all isSpace (unpack s))
    && (case parseHeadingMarker s of
          Just _  => False
          Nothing => True)
    && not (isThematicBreakLine s)
    && (case parseCodeFenceOpen s of
          Just _  => False
          Nothing => True)
    && (case parseFencedDivOpen s of
          Just _  => False
          Nothing => True)
    && not (isAttrBlockLine s)
    && not (isBlockCommentStart s)

||| Apply pending Attrs to a Block, replacing its existing attrs by
||| merging the pending ones on top of whatever the block already
||| carried (which is `emptyAttrs` for newly-parsed blocks). For the
||| handful of blocks that don't carry their own Attrs field (e.g.
||| `RawBlock`, `RefDef`) the call is a no-op.
applyAttrsToBlock : Attrs -> Block -> Block
applyAttrsToBlock a b = case b of
  Paragraph existing is        => Paragraph    (mergeAttrs existing a) is
  Heading   existing lvl is    => Heading      (mergeAttrs existing a) lvl is
  BlockQuote existing bs       => BlockQuote   (mergeAttrs existing a) bs
  ListBlock existing s st t is => ListBlock    (mergeAttrs existing a) s st t is
  CodeBlock existing info body => CodeBlock    (mergeAttrs existing a) info body
  ThematicBreak existing       => ThematicBreak (mergeAttrs existing a)
  Div existing bs              => Div          (mergeAttrs existing a) bs
  Table existing cap rs        => Table        (mergeAttrs existing a) cap rs
  FootnoteDef existing l bs    => FootnoteDef  (mergeAttrs existing a) l bs
  other                        => other

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
         (fenceChar : Char)
      -> (fenceLen : Nat)
      -> (body : List String)
      -> (rest : List String)
      -> (List String, List String)
    collectCodeBody _  _ body [] = (reverse body, [])
    collectCodeBody ch n body (l :: ls) =
      if isCodeFenceClose ch n l
        then (reverse body, ls)
        else collectCodeBody ch n (l :: body) ls

    -- Consume fenced-div body lines until the matching close.
    -- Tracks an active code-block fence so `:::` lines inside a code
    -- block don't terminate the div (matches the Djot reference's
    -- "fenced divs span block boundaries; embedded code blocks are
    -- inert" behaviour). Auto-closes at EOF.
    collectDivBody :
         (openerN : Nat)
      -> (inCode : Maybe (Char, Nat))
      -> (body : List String)
      -> (rest : List String)
      -> (List String, List String)
    collectDivBody _ _       body [] = (reverse body, [])
    collectDivBody n Nothing body (l :: ls) =
      if isFencedDivClose n l
        then (reverse body, ls)
        else case parseCodeFenceOpen l of
          Just (ch, m, _) =>
            collectDivBody n (Just (ch, m)) (l :: body) ls
          Nothing         =>
            collectDivBody n Nothing (l :: body) ls
    collectDivBody n (Just (ch, m)) body (l :: ls) =
      if isCodeFenceClose ch m l
        then collectDivBody n Nothing (l :: body) ls
        else collectDivBody n (Just (ch, m)) (l :: body) ls

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

    -- Footnote body collection: same shape as `collectDefList`, but
    -- continuations are *only* indented lines (the body cannot contain
    -- another footnote opener at column 0 — that opens a new
    -- definition). A blank line is consumed as long as the *next*
    -- non-blank line stays indented.
    collectFootnoteBody : List String -> (List String, List String)
    collectFootnoteBody []        = ([], [])
    collectFootnoteBody (l :: ls) =
      if isBlankLine l
        then case ls of
          [] => ([], [])
          (l' :: _) =>
            if isIndentedLine l'
              then let (more, rest) = collectFootnoteBody ls
                    in (l :: more, rest)
              else ([], l :: ls)
        else if isIndentedLine l
          then let (more, rest) = collectFootnoteBody ls
                in (l :: more, rest)
          else ([], l :: ls)

    -- Collect blockquote lines, including Djot's lazy-continuation
    -- form: an unprefixed non-blank paragraph-continuation line
    -- extends the current blockquote when sandwiched between
    -- `>`-prefixed lines (or following one). Stops at the first blank
    -- line, or at a non-prefixed line that opens a new block (heading,
    -- thematic break, code fence, fenced div, attr block, block
    -- comment). Lazy lines are kept verbatim so the recursive inner
    -- blockquote sees them as continuations of its own paragraph.
    collectQuoteBlock : List String -> (List String, List String)
    collectQuoteBlock []        = ([], [])
    collectQuoteBlock (l :: ls) =
      if isQuotePrefixed l
        then let (more, rest) = collectQuoteBlock ls in (l :: more, rest)
        else if isParagraphContinuable l
          then let (more, rest) = collectQuoteBlock ls in (l :: more, rest)
          else ([], l :: ls)

    -- Strip the `> ` prefix from a quoted line; lazy-continuation
    -- lines (no prefix) pass through unchanged so the inner recursive
    -- blockquote can attach them as paragraph continuations at its own
    -- level.
    stripQuoteOrLazy : String -> String
    stripQuoteOrLazy s = if isQuotePrefixed s then stripQuotePrefix s else s

    -- Consume a block-comment run starting at line `x` (already
    -- known to satisfy `isBlockCommentStart`). The run extends to the
    -- first line containing `%}` (inclusive). Returns the input list
    -- with the comment lines stripped.
    skipBlockComment : (x : String) -> (xs : List String) -> List String
    skipBlockComment x xs =
      if hasBlockCommentEnd x
        then xs
        else case xs of
          []          => []
          (y :: rest) => skipBlockComment y rest

    go : (cur : List String) -> (acc : List LineGroup)
       -> List String -> List LineGroup
    go cur acc []        = reverse (flushNormal (reverse cur) acc)
    go cur acc (x :: xs) =
      if isBlockCommentStart x
        then
          let rest = skipBlockComment x xs
              acc' = flushNormal (reverse cur) acc
           in assert_total (go [] acc' rest)
      else if isBlankLine x
        then go [] (flushNormal (reverse cur) acc) xs
        else case parseCodeFenceOpen x of
          Just (ch, n, info) =>
            let (body, rest) = collectCodeBody ch n [] xs
                acc'         = flushNormal (reverse cur) acc
                code         =
                  CodeGroup info (concat (intersperse "\n" body))
             in assert_total (go [] (code :: acc') rest)
          Nothing => case (cur, parseFencedDivOpen x) of
            ([], Just (n, attrs)) =>
              let (body, rest) = collectDivBody n Nothing [] xs
                  inner         = assert_total (groupLines body)
                  acc'          = flushNormal (reverse cur) acc
                  divGroup      = DivGroup attrs inner
               in assert_total (go [] (divGroup :: acc') rest)
            _ =>
              if isQuotePrefixed x && cur == []
              then
                let (quoteLines, rest) =
                      collectQuoteBlock (x :: xs)
                    inner   = map stripQuoteOrLazy quoteLines
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
                else if isAttrBlockLine x
                  then case parseAttrBlockLine x of
                    Just attrs =>
                      let acc' = flushNormal (reverse cur) acc
                       in assert_total
                            (go [] (AttrPrefixGroup attrs :: acc') xs)
                    Nothing => go (x :: cur) acc xs  -- impossible
                  else if isFootnoteOpener x
                    then
                      let (rest, rs) = collectFootnoteBody xs
                          acc'       = flushNormal (reverse cur) acc
                          (label, openerRest) = case parseFootnoteOpener x of
                            Just (l, r) => (l, r)
                            Nothing     => ("", "")
                          body       = openerRest :: map (dropLeadingSpaces 2) rest
                          fn         = FootnoteGroup label body
                       in assert_total (go [] (fn :: acc') rs)
                    else if isDefListOpener x
                      then
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
            && let marks = filter (not . isSpace) (c :: cs)
                in length marks >= 3 && all (== c) marks

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
            bodyBlocks       = assert_total (parseBodyBlocks bodyLs)
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
    assert_total (groupsToBlocks (groupLines ls))

  ||| Build a `FootnoteDef` from a `FootnoteGroup`. The carried body
  ||| (rest-of-opener + dedented continuation lines) is parsed
  ||| recursively as a nested block sequence.
  footnoteGroupToBlock : (label : String) -> List String -> Block
  footnoteGroupToBlock label body =
    let trimmed = case body of
                    (""    :: rest) => rest  -- empty opener-rest
                    bs              => bs
        blocks  = assert_total (groupsToBlocks (groupLines trimmed))
     in FootnoteDef emptyAttrs label blocks

  ||| Convert a LineGroup into a Block. Quote groups recurse.
  ||| `AttrPrefixGroup` is a marker handled by `groupsToBlocks`; reaching
  ||| this case directly means a trailing attribute line with no
  ||| following block — emit as paragraph text so the source survives.
  public export
  groupToBlock : LineGroup -> Block
  groupToBlock (NormalGroup g)    = normalGroupToBlock g
  groupToBlock (QuoteGroup  gs)   =
    BlockQuote emptyAttrs (assert_total (groupsToBlocks gs))
  groupToBlock (CodeGroup info b) =
    CodeBlock emptyAttrs info b
  groupToBlock (TableGroup rows)  = tableGroupToBlock rows
  groupToBlock (DefListGroup rs)  = defListGroupToBlock rs
  groupToBlock (FootnoteGroup l body) = footnoteGroupToBlock l body
  groupToBlock (AttrPrefixGroup _) = Paragraph emptyAttrs []
  groupToBlock (DivGroup attrs gs) =
    Div attrs (assert_total (groupsToBlocks gs))

  ||| Convert a list of LineGroups into Blocks, threading
  ||| `AttrPrefixGroup`s into the Attrs of the next non-attribute block.
  ||| Trailing attribute prefixes are dropped (no block to attach to).
  public export
  groupsToBlocks : List LineGroup -> List Block
  groupsToBlocks = go emptyAttrs
    where
      isEmpty : Attrs -> Bool
      isEmpty (MkAttrs Nothing [] []) = True
      isEmpty _                       = False

      go : Attrs -> List LineGroup -> List Block
      go _      []                            = []
      go pend (AttrPrefixGroup a :: gs)       =
        go (mergeAttrs pend a) gs
      go pend (g :: gs)                       =
        let b = assert_total (groupToBlock g)
         in (if isEmpty pend then b else applyAttrsToBlock pend b)
            :: assert_total (go emptyAttrs gs)

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
      raw   = groupsToBlocks (groupLines ls)
      table = collectRefDefs raw
      resolved = resolveBlocks table raw
   in Right (MkDoc resolved)
