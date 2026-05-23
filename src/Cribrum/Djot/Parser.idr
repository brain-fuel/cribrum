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
--------------------------------------------------------------------------------

||| Group consecutive non-blank lines; blank lines are separators and produce
||| no group. Order is preserved.
public export
groupLines : List String -> List (List1 String)
groupLines = go [] []
  where
    flush : List String -> List (List1 String) -> List (List1 String)
    flush []           acc = acc
    flush (l :: ls)    acc = (l ::: ls) :: acc

    go : (cur : List String) -> (acc : List (List1 String))
       -> List String -> List (List1 String)
    go cur acc []        = reverse (flush (reverse cur) acc)
    go cur acc (x :: xs) =
      if isBlankLine x
        then go [] (flush (reverse cur) acc) xs
        else go (x :: cur) acc xs

--------------------------------------------------------------------------------
-- Group -> Block.
--------------------------------------------------------------------------------

||| Convert one non-blank line group into a block.
|||
||| A group whose *first* line is an ATX heading produces a heading block
||| (current slice: heading-only groups; multi-line headings arrive later).
||| Everything else is a paragraph.
groupToBlock : List1 String -> Block
groupToBlock (l ::: ls) = case parseHeadingMarker l of
  Just (lvl, rest) =>
    if isNil ls
      then Heading emptyAttrs lvl (parseInlineLine rest)
      else Paragraph emptyAttrs (parseParagraphLines (l ::: ls))
  Nothing => Paragraph emptyAttrs (parseParagraphLines (l ::: ls))

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
