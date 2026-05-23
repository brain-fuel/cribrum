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
-- Inline parser (slice: plain text only — emphasis et al. arrive later).
--------------------------------------------------------------------------------

||| Parse a single line's inline content. Slice v1: the whole line becomes one
||| `InlText`, except an empty string yields no inlines.
parseInlineLine : String -> List Inline
parseInlineLine "" = []
parseInlineLine s  = [InlText s]

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
data LineGroup
  = NormalGroup (List1 String)
  | QuoteGroup  (List LineGroup)

||| `True` iff the line starts with `>` followed by space, OR is exactly `>`
||| (an empty quote line — Djot allows this).
isQuotePrefixed : String -> Bool
isQuotePrefixed s = case unpack s of
  ('>' :: ' ' :: _) => True
  ['>']             => True
  _                 => False

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

||| Group consecutive non-blank lines + recognise block-quote runs.
||| Blank lines outside of a quote run are separators and produce no group.
||| A quote run is the longest prefix of `>`-prefixed lines; its stripped
||| interior is recursively grouped so the quote may itself contain
||| paragraphs, headings, thematic breaks, and (recursively) more quotes.
public export
groupLines : List String -> List LineGroup
groupLines xs = go [] [] xs
  where
    flushNormal : List String -> List LineGroup -> List LineGroup
    flushNormal []           acc = acc
    flushNormal (l :: ls)    acc = NormalGroup (l ::: ls) :: acc

    go : (cur : List String) -> (acc : List LineGroup)
       -> List String -> List LineGroup
    go cur acc []        = reverse (flushNormal (reverse cur) acc)
    go cur acc (x :: xs) =
      if isBlankLine x
        then go [] (flushNormal (reverse cur) acc) xs
        else if isQuotePrefixed x
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

||| Convert one NORMAL line group into a block.
|||
||| Order matters: thematic break is checked first (a single `---` line is
||| not a heading and not a paragraph). Heading is checked next; everything
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
           Nothing => Paragraph emptyAttrs (parseParagraphLines (l ::: ls))

||| Convert a LineGroup into a Block. Quote groups recurse.
public export
groupToBlock : LineGroup -> Block
groupToBlock (NormalGroup g)  = normalGroupToBlock g
groupToBlock (QuoteGroup  gs) =
  BlockQuote emptyAttrs (assert_total (map groupToBlock gs))

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
