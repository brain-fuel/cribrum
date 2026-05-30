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
import Data.Maybe
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

||| Recognise a raw-format attribute immediately following an inline
||| verbatim span: `{=FORMAT}` where FORMAT is a single non-empty token
||| with no internal whitespace (e.g. `` `<a>`{=html} ``). Returns
||| `(format, rest-after-the-brace)`, or `Nothing` if `cs` does not open
||| with such an attribute. A brace carrying anything beyond the bare
||| `=FORMAT` token (e.g. `{=html #id}`) is rejected, so it falls back to
||| an ordinary verbatim span — matching the reference renderer.
takeRawFormatAttr : List Char -> Maybe (String, List Char)
takeRawFormatAttr ('{' :: '=' :: rest) = case findClose '}' rest of
  Just (inner, after) =>
    if inner /= [] && not (any isSpace inner)
      then Just (pack inner, after)
      else Nothing
  Nothing => Nothing
takeRawFormatAttr _ = Nothing

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

||| Like `findClose`, but backslash-aware: a `\` and the character it
||| escapes are carried into the inner content verbatim, so an escaped
||| delimiter (`\_`, `\*`) never counts as the closer. Used for emphasis
||| and strong, where Djot honours backslash escapes (verbatim does not,
||| so that path keeps the plain `findClose`).
findCloseEsc : Char -> List Char -> Maybe (List Char, List Char)
findCloseEsc _ []                = Nothing
findCloseEsc c ('\\' :: y :: xs) = case findCloseEsc c xs of
  Just (ins, rest) => Just ('\\' :: y :: ins, rest)
  Nothing          => Nothing
findCloseEsc c (x :: xs) =
  if x == c
    then Just ([], xs)
    else case findCloseEsc c xs of
      Just (ins, rest) => Just (x :: ins, rest)
      Nothing          => Nothing

||| Collapse a reference label to its matching key: internal whitespace
||| runs (spaces, tabs, newlines from continuation lines) fold to a
||| single space, and leading/trailing whitespace is trimmed. Djot
||| reference matching is case-sensitive but whitespace-insensitive, so
||| `[a and\nb]` matches the refdef `[a and b]` (corpus -009) while
||| `[Link]` does NOT match `[link]` (corpus -012).
normalizeLabelKey : String -> String
normalizeLabelKey = pack . go False . unpack . trim
  where
    -- `pending` is True once a whitespace run has been seen but not yet
    -- emitted; the single collapsing space is written lazily before the
    -- next non-space char. Structurally recursive on the char list.
    go : Bool -> List Char -> List Char
    go _       []        = []
    go pending (c :: cs) =
      if isSpace c
        then go True cs
        else if pending then ' ' :: c :: go False cs
                        else c :: go False cs

||| Flatten parsed inlines to their plain-text content, used to derive
||| the matching key for a collapsed reference (`[link _and_ link][]` ->
||| `link and link`). Mirrors the alt-text flattening in the elaborator:
||| structural markers contribute their children's text, breaks become a
||| space, leaf non-text forms contribute nothing to the key.
inlinesText : List Inline -> String
inlinesText = concatMap one
  where
    one : Inline -> String
    one (InlText s)        = s
    one InlSoftBreak       = " "
    one InlHardBreak       = " "
    one (InlEmph xs)       = assert_total (inlinesText xs)
    one (InlStrong xs)     = assert_total (inlinesText xs)
    one (InlHighlight xs)  = assert_total (inlinesText xs)
    one (InlSuper xs)      = assert_total (inlinesText xs)
    one (InlSub xs)        = assert_total (inlinesText xs)
    one (InlInsert xs)     = assert_total (inlinesText xs)
    one (InlDelete xs)     = assert_total (inlinesText xs)
    one (InlSpan _ xs)     = assert_total (inlinesText xs)
    one (InlLink _ _ xs)   = assert_total (inlinesText xs)
    one (InlImage _ _ xs)  = assert_total (inlinesText xs)
    one (InlVerbatim _ s)  = s
    one _                  = ""

||| Flush an accumulator of plain characters to an `InlText` (singleton
||| or empty). The accumulator is held in reverse order; flushing
||| reverses + packs.
flushAcc : List Char -> List Inline
flushAcc []  = []
flushAcc acc = [InlText (pack (reverse acc))]

||| `True` iff `c` is an ASCII punctuation character — exactly the set
||| Djot lets a backslash escape (``!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~``).
||| A backslash before one of these yields the literal character (and
||| suppresses any smart-punctuation / markup interpretation of it); a
||| backslash before anything else stays a literal backslash.
isAsciiPunct : Char -> Bool
isAsciiPunct c =
  let n = ord c
   in (n >= 0x21 && n <= 0x2F)   -- ! " # $ % & ' ( ) * + , - . /
   || (n >= 0x3A && n <= 0x40)   -- : ; < = > ? @
   || (n >= 0x5B && n <= 0x60)   -- [ \ ] ^ _ `
   || (n >= 0x7B && n <= 0x7E)   -- { | } ~

||| Normalise a link/image destination harvested between `(` and `)`.
||| Two Djot rules apply: (1) backslash escapes are processed, so `\*`
||| in a URL yields a literal `*` (a backslash before a non-punctuation
||| char stays literal); (2) a soft line break inside the destination is
||| removed with no replacement, so a URL spanning continuation lines
||| joins seam-to-seam (`url\nandurl` -> `urlandurl`, `hello *a\nb*` ->
||| `hello *ab*`). Internal spaces are preserved.
normalizeUrl : List Char -> String
normalizeUrl = pack . go
  where
    go : List Char -> List Char
    go []                = []
    go ('\\' :: p :: cs) =
      if isAsciiPunct p then p :: go cs else '\\' :: go (p :: cs)
    go ('\n' :: cs)      = go cs
    go (c :: cs)         = c :: go cs

||| Like `findClose ']'`, but bracket-balanced: nested `[...]` pairs
||| (including image openers `![`) inside the label are skipped over so
||| the closer returned is the one matching the OUTER `[`. Backslash
||| escapes are honoured (an escaped `\[` / `\]` never shifts the depth
||| and the `\`+char are carried into the body verbatim). Used for link
||| and image labels so `[![alt](img)](url)`, `[[foo](bar)](baz)`, and
||| `![[link](url)](img)` find the correct outer `]`.
findCloseBracket : List Char -> Maybe (List Char, List Char)
findCloseBracket = go 0
  where
    go : Nat -> List Char -> Maybe (List Char, List Char)
    go _ []                  = Nothing
    go d ('\\' :: y :: xs)   = case go d xs of
      Just (ins, rest) => Just ('\\' :: y :: ins, rest)
      Nothing          => Nothing
    go d ('[' :: xs)         = case go (S d) xs of
      Just (ins, rest) => Just ('[' :: ins, rest)
      Nothing          => Nothing
    go (S d) (']' :: xs)     = case go d xs of
      Just (ins, rest) => Just (']' :: ins, rest)
      Nothing          => Nothing
    go Z (']' :: xs)         = Just ([], xs)
    go d (x :: xs)           = case go d xs of
      Just (ins, rest) => Just (x :: ins, rest)
      Nothing          => Nothing

||| Split a leading run of identical characters `m` off `cs`, returning
||| (runLength, rest). The first char is assumed already consumed, so the
||| count starts at 1.
spanRun : Char -> List Char -> (Nat, List Char)
spanRun m cs = go 1 cs
  where
    go : Nat -> List Char -> (Nat, List Char)
    go n (c :: rest) = if c == m then go (S n) rest else (n, c :: rest)
    go n []          = (n, [])

||| Render a run of `n` hyphens as Djot smart dashes. Djot's rule:
||| a run divisible by 3 is all em-dashes; else divisible by 2 is all
||| en-dashes; else (n ≡ 2 mod 3) is `(n-2)/3` em-dashes + one en-dash;
||| else (n ≡ 1 mod 3) is `(n-4)/3` em-dashes + two en-dashes. A single
||| hyphen (n = 1) is left literal.
dashRun : Nat -> List Inline
dashRun 0 = []
dashRun 1 = [InlText "-"]
dashRun n =
  let i  = the Integer (cast n)
      m3 = i `mod` 3
      m2 = i `mod` 2
   in if m3 == 0
        then replicate (integerToNat (i `div` 3)) (InlSmart EmDash)
        else if m2 == 0
          then replicate (integerToNat (i `div` 2)) (InlSmart EnDash)
          else if m3 == 2
            then replicate (integerToNat ((i - 2) `div` 3)) (InlSmart EmDash)
                   ++ [InlSmart EnDash]
            else replicate (integerToNat ((i - 4) `div` 3)) (InlSmart EmDash)
                   ++ [InlSmart EnDash, InlSmart EnDash]

||| `True` iff the preceding character (head of the reversed accumulator
||| `acc`) is "open-ish" — start-of-run, whitespace, or an opening
||| punctuation form (`(`, `[`, `{`). This is the left side of the smart
||| quote flanking test.
beforeOpens : List Char -> Bool
beforeOpens []       = True
beforeOpens (c :: _) = isSpace c || c == '(' || c == '[' || c == '{'

||| `True` iff the following character (head of `cs`) is "close-ish" —
||| end-of-run, whitespace, or a closing punctuation form. This is the
||| right side of the flanking test; a quote followed by such a char
||| cannot open.
afterCloses : List Char -> Bool
afterCloses []       = True
afterCloses (c :: _) =
  isSpace c || c == ')' || c == ']' || c == '}'
  || c == '.' || c == ',' || c == '!' || c == '?' || c == ';' || c == ':'

||| `True` iff the run beginning at `cs` is a Djot elision contraction
||| (`'tis`, `'twas`, `'em`, `'n'`, `'cause`, …) where a leading single
||| quote is the apostrophe (right single quote) rather than an opener.
||| `cs` is the character list immediately after the `'`.
isElision : List Char -> Bool
isElision cs =
  let w = pack (map toLower (takeWhile isAlpha cs))
   in w == "tis" || w == "twas" || w == "twere" || w == "til"
   || w == "round" || w == "bout" || w == "cause" || w == "em"
   || w == "n" || w == "nuff"

||| Decide a double-quote (`"`) orientation. It opens (left double quote)
||| only when the char before is open-ish and the char after is not
||| close-ish; otherwise it closes.
doubleQuote : List Char -> List Char -> SmartPunct
doubleQuote acc cs =
  if beforeOpens acc && not (afterCloses cs) then LDQuote else RDQuote

||| Decide a single-quote (`'`) orientation. Djot apostrophe special
||| cases win first: a `'` directly before a digit (`'70s`) or an elision
||| word (`'tis`) is always a right single quote. Otherwise it opens only
||| when the char before is open-ish and the char after is not close-ish.
singleQuote : List Char -> List Char -> SmartPunct
singleQuote acc cs =
  let followedByDigit = case cs of
                          (d :: _) => isDigit d
                          []       => False
   in if followedByDigit || isElision cs
        then RSQuote
        else if beforeOpens acc && not (afterCloses cs)
          then LSQuote
          else RSQuote

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
  parseLinkBody chars = case findCloseBracket chars of
    Just (label, afterClose) =>
      if label == []
        then Nothing
        else case afterClose of
          ('(' :: rest) => case findClose ')' rest of
            Just (url, after) =>
              if url == []
                then Nothing
                else
                  -- Per Djot, an inline-link URL has backslash escapes
                  -- processed and soft line breaks removed (see
                  -- `normalizeUrl`); e.g. `[link](url\nandurl)` ->
                  -- href="urlandurl" (corpus links-and-images-006),
                  -- `[closed](hello\*)` -> href="hello*" (-021).
                  let urlStripped = normalizeUrl url
                      inner = assert_total (parseInlines label)
                   in Just (InlLink emptyAttrs
                              (LinkInline urlStripped Nothing) inner
                          , after)
            Nothing => Nothing
          ('[' :: rest) => case findCloseBracket rest of
            Just (refLabel, after) =>
              -- Empty ref body = collapsed form; the visible text's
              -- plain rendering doubles as the reference label. A
              -- non-empty body is a full reference whose raw bracket
              -- source is the label. Both are whitespace-normalised so
              -- a label spanning continuation lines still matches a
              -- single-line refdef (corpus -009, -015).
              let inner  = assert_total (parseInlines label)
                  label' = if refLabel == []
                             then normalizeLabelKey (inlinesText inner)
                             else normalizeLabelKey (pack refLabel)
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
  parseImageBody chars = case findCloseBracket chars of
    Just (label, afterClose) => case afterClose of
      ('(' :: rest) => case findClose ')' rest of
        Just (url, after) =>
          if url == []
            then Nothing
            else
              let urlStripped = normalizeUrl url
                  inner = assert_total (parseInlines label)
               in Just ( InlImage emptyAttrs
                           (LinkInline urlStripped Nothing) inner
                       , after)
        Nothing => Nothing
      -- Reference image: `![alt][ref]` (full) or `![alt][]` (collapsed,
      -- alt's plain text is the label). Resolved against refdefs in the
      -- two-pass walk; an unresolved ref keeps the InlImage so the alt
      -- still renders (corpus -002, -023).
      ('[' :: rest) => case findCloseBracket rest of
        Just (refLabel, after) =>
          let inner  = assert_total (parseInlines label)
              label' = if refLabel == []
                         then normalizeLabelKey (inlinesText inner)
                         else normalizeLabelKey (pack refLabel)
           in Just ( InlImage emptyAttrs (LinkReference label') inner
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
    -- Backslash escape (Djot): `\` + an ASCII-punctuation char emits that
    -- character literally and suppresses any markup / smart-punctuation
    -- meaning it would otherwise carry. The escaped char joins the plain
    -- accumulator so a later marker scan never sees it. A backslash before
    -- a non-punctuation char (or at end of the run) stays literal.
    '\\' => case cs of
      (p :: rest) =>
        if isAsciiPunct p
          then assert_total (parseInlinesAcc (p :: acc) rest)
          else assert_total (parseInlinesAcc ('\\' :: acc) cs)
      [] => flushAcc ('\\' :: acc)
    -- Emphasis / strong flanking rule (Djot): the marker is emphasised
    -- iff the opener and closer agree on their adjacent-whitespace
    -- status. Both have inside-whitespace (`_ a _`) → emphasis; both
    -- have non-whitespace inside (`_a_`) → emphasis; asymmetric
    -- (`_ a_` or `_a _`) → marker stays literal. Empty body always
    -- fails. On any rule failure the marker joins the plain-text
    -- accumulator and parsing continues.
    '_' => case findCloseEsc '_' cs of
      Just (inner, after) =>
        if inner == [] || openerBlocked cs /= closerBlocked inner
          then assert_total (parseInlinesAcc ('_' :: acc) cs)
          else flushAcc acc
            ++ [InlEmph (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('_' :: acc) cs)
    '*' => case findCloseEsc '*' cs of
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
      -- Braced superscript/subscript (`{^...^}` / `{~...~}`). The braces
      -- let the body carry leading/trailing whitespace (`H{~2 ~}O` ->
      -- `H<sub>2 </sub>O`). A missing two-char closer leaves the `{`
      -- literal so other `{...}` text survives untouched.
      ('^' :: rest) => case findClose2 '^' '}' rest of
        Just (inner, after) =>
          if inner == []
            then assert_total (parseInlinesAcc ('{' :: acc) cs)
            else flushAcc acc
              ++ [InlSuper (assert_total (parseInlinesAcc [] inner))]
              ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('{' :: acc) cs)
      ('~' :: rest) => case findClose2 '~' '}' rest of
        Just (inner, after) =>
          if inner == []
            then assert_total (parseInlinesAcc ('{' :: acc) cs)
            else flushAcc acc
              ++ [InlSub (assert_total (parseInlinesAcc [] inner))]
              ++ assert_total (parseInlinesAcc [] after)
        Nothing => assert_total (parseInlinesAcc ('{' :: acc) cs)
      _ => assert_total (parseInlinesAcc ('{' :: acc) cs)
    '`' =>
      let (more, afterOpen) = takeBacktickRun cs
          openerLen         = S (length more)
       in case findVerbatimClose openerLen afterOpen of
            Just (inner, after) =>
              if inner == []
                then assert_total (parseInlinesAcc ('`' :: acc) cs)
                else case takeRawFormatAttr after of
                  -- `` `…`{=fmt} `` — raw inline of the named format. The
                  -- elaborator gates on `fmt` (html injects, else suppressed).
                  Just (fmt, after') =>
                    flushAcc acc
                      ++ [InlRaw fmt (pack (verbatimStrip inner))]
                      ++ assert_total (parseInlinesAcc [] after')
                  Nothing =>
                    flushAcc acc
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
    -- Superscript (`^...^`) and subscript (`~...~`). Unlike emphasis,
    -- Djot imposes no whitespace-flanking restriction on these markers:
    -- the only requirement is a matching closer and a non-empty body
    -- (e.g. `mc^2^`, `H~2~O`, and nested `^... ~...~^`). On a missing
    -- closer or an empty body the marker stays literal.
    '^' => case findClose '^' cs of
      Just (inner, after) =>
        if inner == []
          then assert_total (parseInlinesAcc ('^' :: acc) cs)
          else flushAcc acc
            ++ [InlSuper (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('^' :: acc) cs)
    '~' => case findClose '~' cs of
      Just (inner, after) =>
        if inner == []
          then assert_total (parseInlinesAcc ('~' :: acc) cs)
          else flushAcc acc
            ++ [InlSub (assert_total (parseInlinesAcc [] inner))]
            ++ assert_total (parseInlinesAcc [] after)
      Nothing => assert_total (parseInlinesAcc ('~' :: acc) cs)
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
            -- An email autolink (`@` present, no scheme `:`) gets a
            -- `mailto:` href while its visible text stays the bare
            -- address (corpus links-and-images-025); a URL autolink
            -- uses the body verbatim for both href and text.
            let text    = pack inner
                isEmail = any (== '@') inner && not (any (== ':') inner)
                url     = if isEmail then "mailto:" ++ text else text
             in flushAcc acc
                  ++ [InlLink emptyAttrs (LinkAuto url) [InlText text]]
                  ++ assert_total (parseInlinesAcc [] after)
          else assert_total (parseInlinesAcc ('<' :: acc) cs)
      Nothing => assert_total (parseInlinesAcc ('<' :: acc) cs)
    -- Smart punctuation: dash runs, ellipsis, and orientation-aware
    -- curly quotes. The full hyphen run is consumed and split into
    -- em-/en-dashes per `dashRun`; a lone hyphen stays literal.
    '-' => case spanRun '-' cs of
      (n, rest) =>
        if n == 1
          then assert_total (parseInlinesAcc ('-' :: acc) cs)
          else flushAcc acc ++ dashRun n
                 ++ assert_total (parseInlinesAcc [] rest)
    '.' => case cs of
      ('.' :: '.' :: rest) =>
        flushAcc acc ++ [InlSmart Ellipsis]
          ++ assert_total (parseInlinesAcc [] rest)
      _ => assert_total (parseInlinesAcc ('.' :: acc) cs)
    '"' =>
      flushAcc acc ++ [InlSmart (doubleQuote acc cs)]
        ++ assert_total (parseInlinesAcc [] cs)
    '\'' =>
      flushAcc acc ++ [InlSmart (singleQuote acc cs)]
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
  ||| A reference-link definition `[label]: url ...` where `url` may
  ||| span indented continuation lines (joined with internal
  ||| whitespace stripped). `rawBody` is the post-`:` content with
  ||| continuation lines concatenated; the title parser runs over it
  ||| at `groupToBlock` time.
  RefDefGroup : (label : String) -> (rawBody : String) -> LineGroup
  ||| A list run: the opening list-item line plus all continuation
  ||| lines (indented continuations, sub-items, and blank lines that
  ||| bridge into a still-open item). Carried verbatim so the
  ||| indentation-aware list parser can split items, detect tight vs
  ||| loose, recover nested sublists, and recurse into item bodies.
  ListGroup : List1 String -> LineGroup

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

||| Forward declaration: the row-cell splitter is defined in the
||| pipe-tables slice further down, but `isTableLine` (and the block
||| grouper) need it here.
splitTableCells : String -> Maybe (List String)

||| `True` iff `s` is a well-formed pipe-table row: the trimmed line
||| both begins and ends with an unescaped, non-verbatim `|`, delimiting
||| at least one cell. A line that starts with `|` but lacks a real
||| closing `|` (e.g. ``| `a |` `` — the second bar lives inside a code
||| span) is NOT a table row and falls through to paragraph parsing.
isTableLine : String -> Bool
isTableLine s = isJust (splitTableCells s)

||| `True` iff `s` is a definition-list opener: `: ` (colon-space) or
||| `:` alone (column-0; no leading indent). Lines that merely begin
||| with `:` but lack the trailing space (like `:emoji:` symbols inside
||| a paragraph) do not open a def list.
isDefListOpener : String -> Bool
isDefListOpener s = case unpack s of
  (':' :: ' ' :: _) => True
  [':']             => True
  _                 => False

||| Per Djot spec: a thematic break is a line consisting of three or
||| more `-` or `*` characters, optionally separated by whitespace,
||| alone on the line. Hoisted ahead of `groupLines` so the blockquote
||| lazy-continuation gate can disqualify thematic-break lines from
||| lazy continuation; also the single canonical source so the
||| mutation gate has one site to mutate (avoiding a duplicate body
||| at the original later position).
isThematicBreak : String -> Bool
isThematicBreak s =
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

--------------------------------------------------------------------------------
-- List markers (number-style + delimiter recognition).
--
-- Djot ordered lists support five number styles (decimal, lower/upper
-- roman, lower/upper alpha) crossed with three delimiters (`1.`, `1)`,
-- `(1)`). Unordered lists use `-`, `*`, `+`. A list ends when the next
-- item's marker has a different number style or delimiter (per the
-- reference parser); a single ambiguous roman/alpha letter (`i`, `v`,
-- `x`, ...) is resolved by looking at the rest of the items, defaulting
-- to roman when still ambiguous.
--------------------------------------------------------------------------------

||| Ordered-list delimiter shape.
data Delim = DPeriod | DParen | DParens

Eq Delim where
  DPeriod == DPeriod = True
  DParen  == DParen  = True
  DParens == DParens = True
  _       == _       = False

||| Possible interpretations of a parsed ordered marker's number style.
||| A pure-decimal marker is `PDecimal`; a single roman/alpha-ambiguous
||| letter is `PAmbig` (case carried); an unambiguous roman is `PRoman`;
||| an unambiguous alpha is `PAlpha`.
data PossStyle
  = PDecimal
  | PRoman Bool
  | PAlpha Bool
  | PAmbig Bool

||| A parsed ordered-list marker.
record OrdMarker where
  constructor MkOrd
  poss  : PossStyle
  delim : Delim
  start : Nat
  core  : List Char  -- the alphanumeric core, for style-dependent start
  width : Nat  -- characters consumed including the trailing space

||| Roman-numeral letter value (lower or upper), or 0 if not a roman letter.
romanVal : Char -> Nat
romanVal c = case toLower c of
  'i' => 1
  'v' => 5
  'x' => 10
  'l' => 50
  'c' => 100
  'd' => 500
  'm' => 1000
  _   => 0

||| `True` iff `c` is a roman-numeral letter.
isRomanLetter : Char -> Bool
isRomanLetter c = romanVal c > 0

||| Convert a roman-numeral string to its integer value (subtractive
||| notation). Assumes every char is a roman letter.
romanToNat : List Char -> Nat
romanToNat = go
  where
    go : List Char -> Nat
    go []        = 0
    go [c]       = romanVal c
    go (c :: d :: rest) =
      let vc = romanVal c
          vd = romanVal d
       in if vc < vd
            then (vd `minus` vc) + assert_total (go rest)
            else vc + assert_total (go (d :: rest))

||| Alpha-marker value: `a`/`A` = 1 .. `z`/`Z` = 26.
alphaToNat : Char -> Nat
alphaToNat c =
  let lc = toLower c
   in if lc >= 'a' && lc <= 'z'
        then S (integerToNat (cast (ord lc - ord 'a')))
        else 0

||| `True` iff `c` is an ASCII letter.
isAsciiLetter : Char -> Bool
isAsciiLetter c = let lc = toLower c in lc >= 'a' && lc <= 'z'

||| Span leading characters satisfying `p`.
spanWhile : (Char -> Bool) -> List Char -> (List Char, List Char)
spanWhile p []        = ([], [])
spanWhile p (c :: cs) =
  if p c then let (a, b) = spanWhile p cs in (c :: a, b)
         else ([], c :: cs)

||| Classify the alphanumeric core of an ordered marker (no delimiters)
||| into `(PossStyle, start)`. `core` is non-empty.
classifyOrdCore : List Char -> Maybe (PossStyle, Nat)
classifyOrdCore core =
  if all isDigit core
    then Just (PDecimal, integerToNat (cast (pack core)))
    else
      let lower   = all (\c => c == toLower c) core
          allRoman = all isRomanLetter core
       in case core of
            [c] => if isAsciiLetter c
                     then if isRomanLetter c
                            then Just (PAmbig lower, alphaToNat c)
                            else Just (PAlpha lower, alphaToNat c)
                     else Nothing
            _   => if allRoman
                     then Just (PRoman lower, romanToNat core)
                     else Nothing

||| Parse an ordered-list marker at the start of `cs`. Recognises the
||| `(x)` paren form first, then a `core` followed by `.` / `)`. The
||| trailing single space is required and counted in `width`.
parseOrdMarker : List Char -> Maybe (OrdMarker, List Char)
parseOrdMarker ('(' :: rest) =
  let (core, after) = spanWhile isAlphaNum rest
   in case after of
        (')' :: ' ' :: body) => case classifyOrdCore core of
          Just (ps, st) =>
            Just (MkOrd ps DParens st core (length core + 3), body)
          Nothing => Nothing
        _ => Nothing
parseOrdMarker cs =
  let (core, after) = spanWhile isAlphaNum cs
   in case (core, after) of
        ([], _)               => Nothing
        (_, '.' :: ' ' :: body) => case classifyOrdCore core of
          Just (ps, st) => Just (MkOrd ps DPeriod st core (length core + 2), body)
          Nothing       => Nothing
        (_, ')' :: ' ' :: body) => case classifyOrdCore core of
          Just (ps, st) => Just (MkOrd ps DParen st core (length core + 2), body)
          Nothing       => Nothing
        _                     => Nothing

||| A list-item marker: either an unordered bullet or an ordered marker.
data Marker = MUnordered Char | MOrdered OrdMarker

||| Parse a list marker at the start of a *trimmed* (left-stripped)
||| line. Returns `(marker, contentColumn, body)` where `contentColumn`
||| is the number of characters the marker occupies (so the content of
||| continuation lines must be indented by `leadIndent + contentColumn`
||| to belong to this item). `body` is the text after the marker.
parseMarker : List Char -> Maybe (Marker, Nat, String)
parseMarker ('-' :: ' ' :: rest) = Just (MUnordered '-', 2, pack rest)
parseMarker ('*' :: ' ' :: rest) = Just (MUnordered '*', 2, pack rest)
parseMarker ('+' :: ' ' :: rest) = Just (MUnordered '+', 2, pack rest)
parseMarker cs = case parseOrdMarker cs of
  Just (m, body) => Just (MOrdered m, width m, pack body)
  Nothing        => Nothing

||| Count leading spaces of a line.
leadingSpaces : String -> Nat
leadingSpaces s = go 0 (unpack s)
  where
    go : Nat -> List Char -> Nat
    go n (' ' :: cs) = go (S n) cs
    go n _           = n

||| `True` iff `s` (at any indentation) opens a list item.
isListOpener : String -> Bool
isListOpener s = case parseMarker (unpack (pack (drop (leadingSpaces s) (unpack s)))) of
  Just _  => True
  Nothing => False


||| Whether two markers can belong to the same list given a *resolution*
||| of ambiguity is decided later; this is the structural same-family
||| test used while grouping lines (any list opener continues a list
||| run). The actual split-into-distinct-lists logic lives in the list
||| block builder.

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

||| Find the first `]` in `xs` and return `(before, after-the-bracket)`,
||| or `Nothing` if no `]` exists. Hoisted ahead of `groupLines` so the
||| refdef opener detection can share the same helper as the post-group
||| parsing path. (A second top-level definition still appears further
||| down for `parseRefDef`'s callers — fine, identical body; can be
||| collapsed later.)
findCloseRBracketEarly : List Char -> Maybe (List Char, List Char)
findCloseRBracketEarly []           = Nothing
findCloseRBracketEarly (']' :: xs)  = Just ([], xs)
findCloseRBracketEarly (x   :: xs)  = case findCloseRBracketEarly xs of
  Just (ins, rest) => Just (x :: ins, rest)
  Nothing          => Nothing

||| Recognise the opener of a Djot reference definition: `[label]:`
||| optionally followed by URL/title content on the same line. Returns
||| `(label, sameLineRest)` — `sameLineRest` is whatever followed
||| `[label]:` on the opener line (post leading whitespace) so the
||| caller can append indented-continuation lines before parsing the
||| title. Footnote definitions (`[^label]:`) are explicitly excluded
||| so they keep going through the footnote opener path.
parseRefDefOpener : String -> Maybe (String, String)
parseRefDefOpener src = case unpack src of
  ('[' :: '^' :: _) => Nothing
  ('[' :: rest)     => case findCloseRBracketEarly rest of
    Just (label, afterClose) => case afterClose of
      (':' :: rest') =>
        if pack label == ""
          then Nothing
          else case rest' of
            []           => Just (pack label, "")
            (' ' :: m)   => Just (pack label, trim (pack m))
            ('\t' :: m)  => Just (pack label, trim (pack m))
            _            => Nothing
      _ => Nothing
    Nothing => Nothing
  _ => Nothing

||| `True` iff `s` starts with a space (a candidate refdef
||| continuation line). Refdef continuation lines have any positive
||| indent; the body content is harvested by `trim`.
isRefDefContinuation : String -> Bool
isRefDefContinuation s = case unpack s of
  (' ' :: _)  => True
  ('\t' :: _) => True
  _           => False

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
    && not (isThematicBreak s)
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

    -- Greedily collect indented refdef-continuation lines. Stops at
    -- the first blank line or non-indented line; the trimmed body of
    -- each line is the caller's responsibility (concatenated with no
    -- separator to match Djot's "join URL parts" semantics on
    -- links-and-images-004 / -005).
    collectRefDefCont : List String -> (List String, List String)
    collectRefDefCont []        = ([], [])
    collectRefDefCont (l :: ls) =
      if isBlankLine l
        then ([], l :: ls)
        else if isRefDefContinuation l
          then let (more, rest) = collectRefDefCont ls in (l :: more, rest)
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

    -- Greedily collect the continuation of a list run. A continuation
    -- line is kept when it is: a blank that bridges to further list
    -- content; an indented line; a list opener at any indent; or a
    -- column-0 plain line that lazily continues the preceding paragraph
    -- (only when the previous collected line was non-blank). The run
    -- ends at a column-0 plain line that follows a blank line (`prevBlank`).
    -- `prevBlank` records whether the last collected line was blank.
    collectList : (prevBlank : Bool) -> List String -> (List String, List String)
    collectList _ []        = ([], [])
    collectList pb (l :: ls) =
      if isBlankLine l
        then case ls of
          [] => ([], [])
          (l' :: _) =>
            if isIndentedLine l' || isListOpener l'
              then let (more, rest) = collectList True ls
                    in (l :: more, rest)
              else ([], l :: ls)
        else if isIndentedLine l || isListOpener l
          then let (more, rest) = collectList False ls
                in (l :: more, rest)
          else if not pb  -- lazy paragraph continuation at column 0
            then let (more, rest) = collectList False ls
                  in (l :: more, rest)
          else ([], l :: ls)


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
          Nothing =>
            let mDivOpen = if cur == [] then parseFencedDivOpen x else Nothing
                mRefOpen = if cur == [] then parseRefDefOpener x else Nothing
             in case mDivOpen of
                  Just (n, attrs) =>
                    let (body, rest) = collectDivBody n Nothing [] xs
                        inner         = assert_total (groupLines body)
                        acc'          = flushNormal (reverse cur) acc
                        divGroup      = DivGroup attrs inner
                     in assert_total (go [] (divGroup :: acc') rest)
                  Nothing => case mRefOpen of
                    Just (label, sameLineRest) =>
                      let (cont, rest) = collectRefDefCont xs
                          bodyParts    = sameLineRest
                                       :: map (trim . pack . unpack) cont
                          rawBody      = concat bodyParts
                          acc'         = flushNormal (reverse cur) acc
                          rg           = RefDefGroup label rawBody
                       in assert_total (go [] (rg :: acc') rest)
                    Nothing =>
                      if isQuotePrefixed x && cur == []
                        then
                          let (quoteLines, rest) = collectQuoteBlock (x :: xs)
                              inner   = map stripQuoteOrLazy quoteLines
                              acc'    = flushNormal (reverse cur) acc
                              quoted  = QuoteGroup (assert_total (groupLines inner))
                           in assert_total (go [] (quoted :: acc') rest)
                        else if isTableLine x
                          then
                            let (rows, rest) = spanList isTableLine (x :: xs)
                                acc'         = flushNormal (reverse cur) acc
                                table        = case rows of
                                  []         => acc'
                                  (r :: rs)  =>
                                    TableGroup (r ::: rs) :: acc'
                             in assert_total (go [] table rest)
                          else if isAttrBlockLine x
                            then case parseAttrBlockLine x of
                              Just attrs =>
                                let acc' = flushNormal (reverse cur) acc
                                 in assert_total
                                      (go [] (AttrPrefixGroup attrs :: acc') xs)
                              Nothing => go (x :: cur) acc xs
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
                              else if isListOpener x && not (isThematicBreak x)
                                                      && isNil cur
                                then
                                  let (rest, rs) = collectList False xs
                                      acc'       = flushNormal (reverse cur) acc
                                      listG      = ListGroup (x ::: rest)
                                   in assert_total (go [] (listG :: acc') rs)
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

||| The list-level descriptor derived from a marker for same-list
||| comparison: an unordered list is keyed by its bullet character; an
||| ordered list by its resolved number-style + delimiter. Two adjacent
||| items continue the same list iff their descriptors are equal (per
||| the reference parser, a change of bullet char or ordered
||| style/delimiter starts a fresh list).
data ListKind
  = KUnordered Char
  | KOrdered PossStyle Delim

||| The `ListStyle` (surface enum) implied by a resolved `PossStyle`.
||| An ambiguous single roman/alpha letter defaults to roman.
possToStyle : PossStyle -> ListStyle
possToStyle PDecimal      = OrderedDecimal
possToStyle (PRoman True)  = OrderedRomanLower
possToStyle (PRoman False) = OrderedRomanUpper
possToStyle (PAlpha True)  = OrderedAlphaLower
possToStyle (PAlpha False) = OrderedAlphaUpper
possToStyle (PAmbig True)  = OrderedRomanLower
possToStyle (PAmbig False) = OrderedRomanUpper

||| Combine two possible-style classifications of adjacent ordered
||| markers into the narrower interpretation, used to resolve a leading
||| ambiguous roman/alpha letter against later items. Returns `Nothing`
||| when the two are genuinely incompatible (different number families)
||| — that breaks the list. Decimal only unifies with decimal.
unifyPoss : PossStyle -> PossStyle -> Maybe PossStyle
unifyPoss PDecimal PDecimal = Just PDecimal
unifyPoss (PRoman l) (PRoman _) = Just (PRoman l)
unifyPoss (PAlpha l) (PAlpha _) = Just (PAlpha l)
unifyPoss (PAmbig l) (PAmbig _) = Just (PAmbig l)
unifyPoss (PAmbig l) (PRoman _) = Just (PRoman l)
unifyPoss (PRoman l) (PAmbig _) = Just (PRoman l)
unifyPoss (PAmbig l) (PAlpha _) = Just (PAlpha l)
unifyPoss (PAlpha l) (PAmbig _) = Just (PAlpha l)
unifyPoss _ _ = Nothing

||| `True` iff two markers continue the same list. The number-style is
||| resolved leniently (ambiguous roman/alpha letters match either), but
||| the delimiter and bullet character must match exactly.
sameList : Marker -> Marker -> Bool
sameList (MUnordered a) (MUnordered b) = a == b
sameList (MOrdered x) (MOrdered y) =
  delim x == delim y && (case unifyPoss (poss x) (poss y) of
                           Just _  => True
                           Nothing => False)
sameList _ _ = False

||| `True` iff `m` is compatible with a `PossStyle` already resolved for
||| the list. Used once the list's number-style is fixed (e.g. by the
||| second item) so a later item whose only interpretation diverges
||| breaks the list — `I.`/`II.` fixes upper-roman, after which `E.`
||| (alpha-only) starts a new list.
possMatches : PossStyle -> PossStyle -> Bool
possMatches PDecimal      PDecimal     = True
possMatches (PRoman _)    (PRoman _)   = True
possMatches (PRoman _)    (PAmbig _)   = True
possMatches (PAlpha _)    (PAlpha _)   = True
possMatches (PAlpha _)    (PAmbig _)   = True
possMatches (PAmbig _)    p            = case p of
  PDecimal => False
  _        => True
possMatches _             _            = False

||| Resolved-style sibling test: a marker continues the list iff its
||| delimiter matches and its possible-style is compatible with the
||| list's already-resolved style.
sameResolvedList : PossStyle -> Delim -> Marker -> Bool
sameResolvedList _  _ (MUnordered _) = False
sameResolvedList ps d (MOrdered y)   = d == delim y && possMatches ps (poss y)

||| Resolve the surface `ListStyle` of an ordered list given its first
||| item's marker and the (possibly disambiguating) second item, when
||| present.
resolveStyle : Marker -> Maybe Marker -> ListStyle
resolveStyle (MUnordered c) _ = case c of
  '-' => UnorderedDash
  '*' => UnorderedAsterisk
  _   => UnorderedPlus
resolveStyle (MOrdered x) (Just (MOrdered y)) =
  case unifyPoss (poss x) (poss y) of
    Just ps => possToStyle ps
    Nothing => possToStyle (poss x)
resolveStyle (MOrdered x) _ = possToStyle (poss x)

||| The HTML `start` value for an ordered list given its *resolved*
||| `ListStyle` and first marker, or `Nothing` when the value is 1 (the
||| default, which the reference renderer omits). The numeric value is
||| recomputed from the resolved style so an ambiguous leading roman/
||| alpha letter (`i`) yields the right number (roman `i` = 1, alpha
||| `i` = 9). Unordered lists never carry a start.
markerStart : ListStyle -> Marker -> Maybe Nat
markerStart _ (MUnordered _) = Nothing
markerStart style (MOrdered x) =
  let n = case style of
            OrderedRomanLower => romanToNat (core x)
            OrderedRomanUpper => romanToNat (core x)
            OrderedAlphaLower => case core x of (c :: _) => alphaToNat c; [] => start x
            OrderedAlphaUpper => case core x of (c :: _) => alphaToNat c; [] => start x
            _                 => start x
   in if n == 1 then Nothing else Just n

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

||| Strip a leading heading marker (any level 1..6) from a line,
||| returning the content after it. Lines with no marker pass through
||| verbatim — they are lazy-continuation lines of an open heading.
stripHeadingMarker : String -> String
stripHeadingMarker s = case parseHeadingMarker s of
  Just (_, rest) => rest
  Nothing        => s

||| Build a heading's inline content from its opener plus lazy-
||| continuation lines. Each line's heading marker (if any) is stripped;
||| the remaining text is joined with `\n` (the inline tokenizer turns
||| that into a soft break) and parsed as inlines. Lines that strip to
||| empty (a bare marker) contribute nothing, matching the reference's
||| `## \n heading -> <h2>heading</h2>` shape.
parseHeadingLines : List String -> List Inline
parseHeadingLines lines =
  parseInlines (joinNL (filter (/= "") (map stripHeadingMarker lines)))
  where
    joinNL : List String -> List Char
    joinNL []        = []
    joinNL [s]       = unpack s
    joinNL (s :: ss) = unpack s ++ ('\n' :: joinNL ss)

||| `True` iff line `s` continues a heading opened at level `lvl`: it is
||| either a plain (non-marker) line — a lazy continuation — or a heading
||| marker of the SAME level. A marker of a *different* level closes the
||| current heading and opens a new one (Djot headings-003/006/008).
continuesHeading : Nat -> String -> Bool
continuesHeading lvl s = case parseHeadingMarker s of
  Just (l', _) => l' == lvl
  Nothing      => True

||| Split a heading-led line run into one or more `Heading` blocks. The
||| opener fixes the level; following lines extend the same heading while
||| `continuesHeading` holds (lazy continuation, same-level markers folded
||| in). The first different-level marker starts a fresh heading, which
||| `elaborate` later nests as a `<section>`.
splitHeadings : (lvl : Nat) -> (opener : String) -> (more : List String) -> List Block
splitHeadings lvl opener more =
  let (cont, rest) = spanList (continuesHeading lvl) more
      hd           = Heading emptyAttrs lvl (parseHeadingLines (opener :: cont))
   in case rest of
        []        => [hd]
        (r :: rs) => case parseHeadingMarker r of
          Just (lvl', _) => hd :: assert_total (splitHeadings lvl' r rs)
          Nothing        => [hd]   -- unreachable: `rest` opens at a marker

||| Convert one NORMAL line group into a block (non-heading path). A
||| heading-led group is handled by `normalGroupToBlocks` instead.
|||
||| Order matters: thematic break is checked first (a single `---` line is
||| not a heading and not a paragraph); then reference definition
||| (single-line `[ref]: url`); everything else falls through to
||| paragraph. Headings are split out by `normalGroupToBlocks`; lists
||| never reach here — they are captured as `ListGroup`s by `groupLines`.
normalGroupToBlock : List1 String -> Block
normalGroupToBlock (l ::: ls) =
  if isNil ls && isThematicBreak l
    then ThematicBreak emptyAttrs
    else if isNil ls
      then case parseRefDef l of
        Just (label, url, title) => RefDef label url title
        Nothing => Paragraph emptyAttrs (parseParagraphLines (l ::: ls))
      else Paragraph emptyAttrs (parseParagraphLines (l ::: ls))

||| Convert one NORMAL line group into a block sequence. A group whose
||| first line is an ATX heading marker yields one or more `Heading`
||| blocks (lazy continuation + level-change splitting via
||| `splitHeadings`); every other group yields a single block.
normalGroupToBlocks : List1 String -> List Block
normalGroupToBlocks (l ::: ls) = case parseHeadingMarker l of
  Just (lvl, _) => splitHeadings lvl l ls
  Nothing       => [normalGroupToBlock (l ::: ls)]

--------------------------------------------------------------------------------
-- Pipe tables (slice).
--------------------------------------------------------------------------------

-- Split a table-row source into trimmed cell strings, respecting inline
-- structure: a `|` that is backslash-escaped (`\|`) or sits inside an
-- inline verbatim/code span (run of backticks closed by a run of the
-- SAME length) does NOT separate cells. Returns `Nothing` when the line
-- is not a well-formed table row (after trimming it must both begin and
-- end with an unescaped, non-verbatim `|`). The leading and trailing bar
-- are stripped; the interior is split on the surviving bars; each cell is
-- whitespace-trimmed. The escaped `\|` stays verbatim in the cell text
-- (the inline parser unescapes it); only its delimiter role is dropped.
-- (Signature is forward-declared earlier, for `isTableLine`.)
splitTableCells line =
  let chars = unpack (trim line)
   in case chars of
        ('|' :: rest) =>
          -- Walk the interior, accumulating cells. Track whether the
          -- final emitted segment was closed by a real trailing `|`.
          -- `length rest` is decreasing fuel that makes the walk total
          -- even when a verbatim span jumps over many chars at once.
          map (map (trim . pack)) (go (length rest) [] [] False rest)
        _ => Nothing
  where
    -- `cur` is the current cell (reversed); `acc` the finished cells
    -- (reversed); `closed` records that the most recent `|` we consumed
    -- was a genuine separator with nothing meaningful after it. The
    -- result is `Just cells` only if the row ended on a separator `|`
    -- (well-formed); otherwise `Nothing`. `fuel` bounds the recursion.
    go : (fuel : Nat) -> (cur : List Char) -> (acc : List (List Char))
       -> (closed : Bool) -> List Char -> Maybe (List (List Char))
    go _ cur acc closed [] =
      -- A well-formed row ends exactly at the trailing `|`: `cur` must
      -- be empty (everything after the last `|` was nothing) and we
      -- must have seen that closing `|`.
      if closed && all (== ' ') cur
        then Just (reverse acc)
        else Nothing
    go Z _ _ _ _ = Nothing  -- fuel exhausted: treat as malformed
    go (S k) cur acc _ ('\\' :: c :: cs) =
      -- Escaped char: keep both chars literally, no delimiter role.
      go k (c :: '\\' :: cur) acc False cs
    go (S k) cur acc _ ['\\'] =
      -- Trailing lone backslash: literal, not a separator close.
      go k ('\\' :: cur) acc False []
    go (S k) cur acc _ xs@('`' :: _) =
      -- Verbatim span: consume up to the matching backtick run so any
      -- `|` inside is inert. Unclosed span runs to end of line.
      let (ticks, after) = takeBacktickRun xs in
      case findVerbatimClose (length ticks) after of
        Just (body, rest) =>
          go k (reverse (ticks ++ body ++ ticks) ++ cur) acc False rest
        Nothing =>
          go k (reverse (ticks ++ after) ++ cur) acc False []
    go (S k) cur acc _ ('|' :: cs) =
      -- Real cell separator. Emit the current cell and start fresh.
      go k [] (reverse cur :: acc) True cs
    go (S k) cur acc _ (c :: cs) =
      go k (c :: cur) acc False cs

||| Classify one alignment-row cell. A cell is an alignment marker iff
||| it consists of a leading optional `:`, ONE OR MORE `-`, and a
||| trailing optional `:` (no other characters). Returns the
||| corresponding `Align`, or `Nothing` if the cell isn't a marker.
||| (Per the Djot spec a single `-` suffices; a bare `:` with no dash
||| is not a marker.)
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
   in if length bar >= 1 && all (== '-') bar
        then Just $ case (left, right) of
          (True,  True)  => AlignCenter
          (True,  False) => AlignLeft
          (False, True)  => AlignRight
          (False, False) => AlignNone
        else Nothing

||| `Just aligns` iff `s` is a well-formed table row whose every cell is
||| an alignment marker AND there is at least one cell. The cell count
||| is the column count of the surrounding table.
parseAlignRow : String -> Maybe (List Align)
parseAlignRow s = case splitTableCells s of
  Just cs@(_ :: _) => traverse parseAlignCell cs
  _                => Nothing

||| Build a `TableCell` from a raw cell source + an alignment.
makeCell : Align -> String -> TableCell
makeCell a body = MkCell a (parseInlineLine body)

||| Build a `TableRow` from a raw row source. If `aligns` is shorter
||| than the cells (or empty), missing positions get `AlignNone`. A row
||| that does not split into cells (malformed) yields an empty row.
makeRow : (isHeader : Bool) -> List Align -> String -> TableRow
makeRow header aligns line =
  let cells = fromMaybe [] (splitTableCells line)
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
||| Rows are processed in source order, threading the current column
||| alignment. An alignment-marker row (`|:-|---:|` …) is not itself a
||| body row: it sets the running alignment and, when an ordinary row
||| sits immediately above it, promotes that row to a header (re-aligned
||| with the markers). This lets a single table interleave multiple
||| header bands (Djot `tables-005`). A leading separator with no row
||| above it (`tables-006`/`-007`) just establishes the alignment.
|||
||| `caption`, when present, is rendered as the table's `<caption>`.
||| Re-pair existing cells with a fresh alignment list (used when a
||| separator row promotes the row directly above it into a header).
||| Extra cells beyond the alignment list keep `AlignNone`.
reAlignCells : List Align -> List TableCell -> List TableCell
reAlignCells _         []        = []
reAlignCells []        (c :: cs) = MkCell AlignNone (content c) :: reAlignCells [] cs
reAlignCells (a :: as) (c :: cs) = MkCell a (content c) :: reAlignCells as cs

tableGroupToBlockCap : (caption : Maybe (List Inline)) -> List1 String
                                                       -> Block
tableGroupToBlockCap caption (l ::: ls) =
  Table emptyAttrs caption (reverse (go [] [] (l :: ls)))
  where
    -- `aligns`   : current column alignment in effect.
    -- `acc`      : finished rows, reversed.
    -- The previous element of `acc` (its head) is the candidate a
    -- following separator promotes to a header.
    go : (aligns : List Align) -> (acc : List TableRow) -> List String
       -> List TableRow
    go _      acc []          = acc
    go aligns acc (x :: xs)   = case parseAlignRow x of
      Just newAligns =>
        -- Separator row: update alignment and, if a row sits directly
        -- above, re-emit it as a re-aligned header.
        let acc' = case acc of
                     (prev :: rest) =>
                       MkRow True (reAlignCells newAligns (cells prev)) :: rest
                     [] => []
         in go newAligns acc' xs
      Nothing =>
        go aligns (makeRow False aligns x :: acc) xs

||| Caption-free convenience wrapper (used where no `^ …` block follows).
tableGroupToBlock : List1 String -> Block
tableGroupToBlock = tableGroupToBlockCap Nothing

--------------------------------------------------------------------------------
-- List item splitting + loose/tight detection.
--------------------------------------------------------------------------------

||| The marker of a line at its own indentation, if any.
lineMarker : String -> Maybe Marker
lineMarker s =
  let ind = leadingSpaces s
   in case parseMarker (drop ind (unpack s)) of
        Just (m, _, _) => Just m
        Nothing        => Nothing

||| `True` iff `x` (a non-blank line) begins a *new sibling item* of a
||| list whose markers sit at `markerIndent`: its indentation is
||| `<= markerIndent` and its marker continues the list per `continues`.
isSiblingMarker : (markerIndent : Nat) -> (continues : Marker -> Bool)
               -> String -> Bool
isSiblingMarker mi continues x = case lineMarker x of
  Just m  => leadingSpaces x <= mi && continues m
  Nothing => False

||| Split a list group's lines into per-item runs plus the leftover
||| lines that belong to a *different* list (a base-level marker of a
||| different family) or fall outside the list entirely. The opener line
||| of each item starts a fresh run; subsequent blank, indented, or lazy
||| (column-0 plain text immediately continuing a paragraph) lines accrue
||| to it. A same-family base-level marker opens the next sibling item.
||| `prevBlank` tracks whether the previous line was blank, so a column-0
||| plain line is treated as a lazy continuation only when it directly
||| follows non-blank content. Single structural pass.
splitItems :
     (markerIndent : Nat) -> (continues : Marker -> Bool)
  -> List String -> (List (List String), List String)
splitItems mi continues allLines = go False [] [] allLines
  where
    flush : List String -> List (List String) -> List (List String)
    flush []  acc = acc
    flush cur acc = reverse cur :: acc

    go : (prevBlank : Bool) -> (cur : List String) -> (acc : List (List String))
       -> List String -> (List (List String), List String)
    go pb cur acc []        = (reverse (flush cur acc), [])
    go pb cur acc (x :: xs) =
      if isBlankLine x
        then go True (x :: cur) acc xs
        else if isSiblingMarker mi continues x
          then go False [x] (flush cur acc) xs   -- next sibling item
          else if leadingSpaces x > mi
            then go False (x :: cur) acc xs       -- indented continuation
            else case lineMarker x of
              -- A base-level marker of a different family ends this list
              -- and starts a new one: hand the remainder back as leftover.
              Just _  => (reverse (flush cur acc), x :: xs)
              Nothing =>
                if not pb
                  then go False (x :: cur) acc xs -- lazy paragraph cont.
                  else (reverse (flush cur acc), x :: xs)  -- blank then text

||| `True` iff a single item's lines contain a blank line that forces
||| the list loose. Trailing blanks are ignored. A blank line that is
||| immediately followed (skipping further blanks) by an indented
||| sub-list opener does NOT force loose — per Djot, a blank line before
||| a nested list keeps the parent tight.
itemHasInnerBlank : List String -> Bool
itemHasInnerBlank run =
  let trimmed = reverse (dropWhile isBlankLine (reverse run))
   in go trimmed
  where
    go : List String -> Bool
    go []        = False
    go (x :: xs) =
      if isBlankLine x
        then case dropWhile isBlankLine xs of
               (y :: _) => if isListOpener y && isIndentedLine y
                             then go xs        -- blank before sub-list: tight
                             else True         -- blank before plain block: loose
               []       => go xs
        else go xs

||| Parse a table caption's lines into inline content: the first line
||| (with its `^ ` marker already stripped) plus continuation lines,
||| joined by soft breaks.
captionInlines : String -> List String -> List Inline
captionInlines first []        = parseInlineLine first
captionInlines first (c :: cs) =
  parseInlineLine first ++ (InlSoftBreak :: captionInlines c cs)

||| If the head group is a caption paragraph (a `NormalGroup` whose first
||| line begins with `^ `), return its parsed inline content plus the
||| remaining groups; otherwise `Nothing` and the groups unchanged. Used
||| to attach a `^ …` block to the pipe table directly above it.
captionFor : List LineGroup -> (Maybe (List Inline), List LineGroup)
captionFor (NormalGroup (l ::: ls) :: gs) =
  case unpack l of
    ('^' :: ' ' :: rest) => (Just (captionInlines (pack rest) ls), gs)
    _                    => (Nothing, NormalGroup (l ::: ls) :: gs)
captionFor gs = (Nothing, gs)

mutual
  ||| Build one or more `ListBlock`s from a `ListGroup`'s raw lines. A
  ||| single group may contain several adjacent lists (when the marker
  ||| family changes, e.g. `-` then `+`, or roman then alpha): the first
  ||| list is built here and the leftover lines are re-grouped. Splits
  ||| into items by indentation, resolves number-style + start, detects
  ||| tight vs loose, and recurses into each item's dedented body.
  ||| Task-list items are recognised when an item body opens with a
  ||| `[ ]` / `[x]` checkbox.
  listGroupToBlocks : List1 String -> List Block
  listGroupToBlocks (l ::: ls) =
    let mi = leadingSpaces l
        afterMi = drop mi (unpack l)
     in case parseMarker afterMi of
          Nothing => [Paragraph emptyAttrs (parseInlineLine l)]  -- impossible
          Just (first, mw, _) =>
            let ci = mi + mw
                -- Phase 1: a lenient split (ambiguous roman/alpha letters
                -- match either) to find the second item, which fixes the
                -- ordered number-style.
                (lenientRuns, _) = splitItems mi (sameList first) (l :: ls)
                secondMarker : Maybe Marker
                secondMarker = case lenientRuns of
                  (_ :: (s :: _) :: _) =>
                    let si = leadingSpaces s
                     in case parseMarker (drop si (unpack s)) of
                          Just (m2, _, _) => Just m2
                          Nothing         => Nothing
                  _ => Nothing
                style0 = resolveStyle first secondMarker
                -- Phase 2: the real split, using the *resolved* style so
                -- a later item whose only interpretation diverges (e.g.
                -- `E.` after roman `I.`/`II.`) breaks the list.
                continues : Marker -> Bool
                continues m = case first of
                  MUnordered _ => sameList first m
                  MOrdered x   => case style0 of
                    OrderedDecimal    => sameResolvedList PDecimal (delim x) m
                    OrderedRomanLower => sameResolvedList (PRoman True)  (delim x) m
                    OrderedRomanUpper => sameResolvedList (PRoman False) (delim x) m
                    OrderedAlphaLower => sameResolvedList (PAlpha True)  (delim x) m
                    OrderedAlphaUpper => sameResolvedList (PAlpha False) (delim x) m
                    _                 => sameList first m
                splitResult = splitItems mi continues (l :: ls)
                itemRuns = fst splitResult
                leftover = snd splitResult
                start0 = markerStart style0 first
                -- Loose if any item is separated from the next by a
                -- blank line, or any item has a loose-forcing inner blank.
                -- A trailing blank that follows a *nested sub-list* line
                -- is absorbed by that sub-list and does not loosen the
                -- outer list (matches the reference parser).
                trailingBlankSeparates : List String -> Bool
                trailingBlankSeparates r =
                  case reverse r of
                    (b :: more) =>
                      isBlankLine b &&
                      (case dropWhile isBlankLine more of
                         (lastNonBlank :: _) =>
                           not (isListOpener lastNonBlank
                                && isIndentedLine lastNonBlank)
                         [] => False)
                    [] => False
                hasSep : List (List String) -> Bool
                hasSep []           = False
                hasSep [_]          = False
                hasSep (r :: rest)  = trailingBlankSeparates r || hasSep rest
                anyInner = any itemHasInnerBlank itemRuns
                loose    = hasSep itemRuns || anyInner
                tight    = not loose
                items    = map (assert_total (mkItem ci tight)) itemRuns
                -- Promote to a TaskList when every item is a checkbox.
                isTaskItem : ListItem -> Bool
                isTaskItem i = case checked i of
                  Just _  => True
                  Nothing => False
                allTask = case items of
                  [] => False
                  _  => all isTaskItem items
                style = if allTask then TaskList else style0
                thisList = ListBlock emptyAttrs style start0 tight items
                more : List Block
                more = case leftover of
                         []        => []
                         (x :: xs) =>
                           assert_total (groupsToBlocks (groupLines (x :: xs)))
             in thisList :: more

  ||| Build one `ListItem` from a single item's raw lines. The opener
  ||| line's marker is stripped; continuation lines are dedented by the
  ||| content indent. A leading `[ ]`/`[x]` checkbox promotes the item
  ||| to a task item (the checkbox is consumed from the body).
  mkItem : (contentIndent : Nat) -> (tight : Bool) -> List String -> ListItem
  mkItem ci tight run = case run of
    [] => MkLI emptyAttrs Nothing Nothing []
    (opener :: rest) =>
      let mi      = leadingSpaces opener
          body0   = case parseMarker (drop mi (unpack opener)) of
                      Just (_, _, b) => b
                      Nothing        => opener
          (checkedFlag, body) = case parseTaskMarker (unpack body0) of
            Just (c, b) => (Just c, b)
            Nothing     => (Nothing, body0)
          contLines = map (dropLeadingSpaces ci) rest
          bodyLines = body :: contLines
          blocks    = assert_total (parseBodyBlocks bodyLines)
       in MkLI emptyAttrs checkedFlag Nothing blocks

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
    -- Djot raw block: a fenced block whose info string is `=FORMAT`
    -- (leading `=`) is raw passthrough of the named format, not a code
    -- block. The elaborator gates on the format (`html` injects literal,
    -- others are suppressed — conventions §1).
    case unpack info of
      ('=' :: fmt) => RawBlock (trim (pack fmt)) b
      _            => CodeBlock emptyAttrs info b
  groupToBlock (TableGroup rows)  = tableGroupToBlock rows
  groupToBlock (DefListGroup rs)  = defListGroupToBlock rs
  groupToBlock (ListGroup rs)     = case listGroupToBlocks rs of
    (b :: _) => b
    []       => Paragraph emptyAttrs []
  groupToBlock (FootnoteGroup l body) = footnoteGroupToBlock l body
  groupToBlock (AttrPrefixGroup _) = Paragraph emptyAttrs []
  groupToBlock (DivGroup attrs gs) =
    Div attrs (assert_total (groupsToBlocks gs))
  groupToBlock (RefDefGroup label rawBody) =
    case extractRefTitle rawBody of
      Just (url, title) => RefDef label url (Just title)
      Nothing           => RefDef label rawBody Nothing

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

      -- Attach pending attrs to the FIRST block only (a heading-led
      -- NormalGroup can expand to several blocks; the prefix binds the
      -- opener).
      attachHead : Attrs -> List Block -> List Block
      attachHead _    []        = []
      attachHead pend (b :: bs) =
        (if isEmpty pend then b else applyAttrsToBlock pend b) :: bs

      go : Attrs -> List LineGroup -> List Block
      go _      []                            = []
      go pend (AttrPrefixGroup a :: gs)       =
        go (mergeAttrs pend a) gs
      go pend (NormalGroup g :: gs)           =
        attachHead pend (normalGroupToBlocks g)
          ++ assert_total (go emptyAttrs gs)
      -- A list group can expand to several adjacent lists; the pending
      -- attribute prefix attaches to the first.
      go pend (ListGroup rs :: gs)            =
        let bs = assert_total (listGroupToBlocks rs)
            bs' = case bs of
                    []        => []
                    (b :: r)  =>
                      (if isEmpty pend then b else applyAttrsToBlock pend b) :: r
         in bs' ++ assert_total (go emptyAttrs gs)
      -- A `^ …` paragraph immediately following a pipe table is its
      -- caption (Djot `tables-004`). Absorb the following NormalGroup
      -- into the table's `<caption>` instead of emitting a paragraph.
      go pend (TableGroup rows :: gs)         =
        let (cap, rest) = captionFor gs
            b = tableGroupToBlockCap cap rows
         in (if isEmpty pend then b else applyAttrsToBlock pend b)
            :: assert_total (go emptyAttrs rest)
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
  collectRefDefs : List Block -> List (String, (String, Maybe String))
  collectRefDefs []        = []
  collectRefDefs (b :: bs) =
    refDefsFromBlock b ++ assert_total (collectRefDefs bs)

  refDefsFromBlock : Block -> List (String, (String, Maybe String))
  refDefsFromBlock (RefDef l u t)      = [(normalizeLabelKey l, (u, t))]
  refDefsFromBlock (BlockQuote _ bs')  =
    assert_total (collectRefDefs bs')
  refDefsFromBlock _                   = []

mutual
  ||| Resolve `LinkReference label` to `LinkInline url title` (link) or
  ||| keep the `InlImage` ref form with the resolved URL when the label
  ||| is known. Unknown labels pass through unchanged so the elaborator
  ||| renders the visible text. Inline/auto forms are left alone.
  resolveInline : List (String, (String, Maybe String)) -> Inline -> Inline
  resolveInline tab i = case i of
    InlLink a (LinkReference l) xs => case lookup l tab of
      Just (url, title) => InlLink a (LinkInline url title)
                    (assert_total (resolveInlines tab xs))
      Nothing  => InlLink a (LinkReference l)
                    (assert_total (resolveInlines tab xs))
    InlImage a (LinkReference l) xs => case lookup l tab of
      Just (url, title) => InlImage a (LinkInline url title)
                    (assert_total (resolveInlines tab xs))
      Nothing  => InlImage a (LinkReference l)
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

  resolveInlines : List (String, (String, Maybe String)) -> List Inline -> List Inline
  resolveInlines tab = map (resolveInline tab)

mutual
  resolveBlock : List (String, (String, Maybe String)) -> Block -> Block
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

  resolveBlocks : List (String, (String, Maybe String)) -> List Block -> List Block
  resolveBlocks tab = map (resolveBlock tab)

  resolveItem : List (String, (String, Maybe String)) -> ListItem -> ListItem
  resolveItem tab (MkLI a c t bs) =
    MkLI a c (map (resolveInlines tab) t)
      (assert_total (resolveBlocks tab bs))

  resolveRow : List (String, (String, Maybe String)) -> TableRow -> TableRow
  resolveRow tab (MkRow h cs) = MkRow h (map (resolveCell tab) cs)

  resolveCell : List (String, (String, Maybe String)) -> TableCell -> TableCell
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
