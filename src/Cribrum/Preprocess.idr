||| `{% include: path %}` source-composition preprocessor (pure core).
|||
||| Djot has no native include/transclude syntax, so Cribrum assembles a
||| whole `.dj` document from snippets as a *textual* pass over the source
||| *before* `parseDoc`. Because the spliced result is one document fed to
||| one `parseDoc -> elaborateDoc`, there is exactly one `<main>` by
||| construction — nested `<main>` (which would fail the `unique-main`
||| structural rule) is impossible at the source level.
|||
||| The directive `{% include: relative/path.dj %}` is a real Djot block
||| comment (`Cribrum.Djot.Parser.isBlockCommentStart`), so an
||| *un-preprocessed* file degrades gracefully: the parser drops the line
||| rather than emitting broken output.
|||
||| This module is the pure, total core (recognition + path normalisation
||| + the resolver-injected recursive splicer). All IO — reading files,
||| building the resolver — lives in `tools/preprocess`, mirroring the
||| `Cribrum.Pipeline.Anchor` (pure lib) / `render-docsnav` (IO tool)
||| split. Keeping the splice logic pure earns it both unit-test and
||| mutation-gate coverage.
module Cribrum.Preprocess

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Char/string helpers (kept local + total; no surprises from stdlib churn).
--------------------------------------------------------------------------------

||| Drop a leading char prefix; `Nothing` if `xs` does not start with it.
stripPrefixCs : List Char -> List Char -> Maybe (List Char)
stripPrefixCs []        ys        = Just ys
stripPrefixCs (x :: xs) (y :: ys) = if x == y then stripPrefixCs xs ys else Nothing
stripPrefixCs (_ :: _)  []        = Nothing

||| Drop a trailing char suffix; `Nothing` if absent.
stripSuffixCs : List Char -> List Char -> Maybe (List Char)
stripSuffixCs suf xs = map reverse (stripPrefixCs (reverse suf) (reverse xs))

||| Split a string on a separator char (no separators in the result).
splitOnChar : Char -> String -> List String
splitOnChar c s = go (unpack s) []
  where
    go : List Char -> List Char -> List String
    go []        cur = [pack (reverse cur)]
    go (x :: xs) cur = if x == c then pack (reverse cur) :: go xs []
                                 else go xs (x :: cur)

||| Join with single '/' separators.
joinSlash : List String -> String
joinSlash []        = ""
joinSlash [x]       = x
joinSlash (x :: xs) = x ++ "/" ++ joinSlash xs

||| Join lines with '\n' (no trailing newline — preserves source shape).
joinNL : List String -> String
joinNL []        = ""
joinNL [x]       = x
joinNL (x :: xs) = x ++ "\n" ++ joinNL xs

--------------------------------------------------------------------------------
-- Directive recognition.
--------------------------------------------------------------------------------

||| If `line` (after trimming) is exactly a `{% include: PATH %}` Djot
||| block comment, return the trimmed PATH. Whole-line directives only;
||| an empty PATH yields `Nothing` (the line stays an ordinary comment).
||| The trimmed line must open with `{%`, close with `%}`, and the inner
||| text must start with `include:`. Spaces around the path are optional.
public export
parseIncludeDirective : String -> Maybe String
parseIncludeDirective line =
  case stripPrefixCs ['{','%'] (unpack (trim line)) of
    Nothing        => Nothing
    Just afterOpen => case stripSuffixCs ['%','}'] afterOpen of
      Nothing   => Nothing
      Just body => case stripPrefixCs (unpack "include:") (unpack (trim (pack body))) of
        Nothing   => Nothing
        Just rest => let path = trim (pack rest) in
                     if path == "" then Nothing else Just path

--------------------------------------------------------------------------------
-- Path normalisation.
--------------------------------------------------------------------------------

||| The directory part of a slash path ("" when there is no slash).
public export
dirname : String -> String
dirname p = case dropWhile (/= '/') (reverse (unpack p)) of
  []          => ""
  (_ :: rest) => pack (reverse rest)

||| Normalise a slash path: drop empty (`//`) and `.` segments, and let
||| `..` pop the previous segment (unless it would climb above a leading
||| `..`). A leading `/` is preserved. Pure + idempotent.
public export
normalizePath : String -> String
normalizePath p =
  let abs   = isPrefixOf "/" p
      stack = foldl step [] (splitOnChar '/' p)
      body  = joinSlash (reverse stack)
   in if abs then "/" ++ body else body
  where
    step : List String -> String -> List String
    step acc ""   = acc
    step acc "."  = acc
    step acc ".." = case acc of
      (top :: rest) => if top == ".." then ".." :: acc else rest
      []            => [".."]
    step acc s    = s :: acc

||| Resolve an include reference against the including file's directory to
||| a canonical key. An empty `curDir` (top-level entry file in the cwd)
||| resolves the reference as-is rather than rooting it at `/`.
public export
resolvePath : (curDir : String) -> (ref : String) -> String
resolvePath curDir ref =
  normalizePath (if curDir == "" then ref else curDir ++ "/" ++ ref)

--------------------------------------------------------------------------------
-- Resolver-injected recursive splice.
--------------------------------------------------------------------------------

||| One resolved include: its canonical key (for cycle detection), the
||| snippet text, and the directory its *own* includes resolve against.
public export
record Resolved where
  constructor MkResolved
  key      : String
  contents : String
  baseDir  : String

||| Caller-supplied resolver: `(currentDir, ref) -> resolved file`. Keeps
||| `spliceWith` free of IO and path-syntax knowledge. `Nothing` = the
||| reference does not resolve to a readable file.
public export
ResolveFn : Type
ResolveFn = String -> String -> Maybe Resolved

public export
data IncludeError
  = MissingInclude String (List String)   -- unresolved ref, ancestor chain
  | IncludeCycle   String (List String)   -- cycling key, ancestor chain
  | DepthExceeded  String (List String)   -- ref hit at fuel 0, ancestor chain

public export
Eq IncludeError where
  MissingInclude a x == MissingInclude b y = a == b && x == y
  IncludeCycle   a x == IncludeCycle   b y = a == b && x == y
  DepthExceeded  a x == DepthExceeded  b y = a == b && x == y
  _                  == _                  = False

||| Render an ancestor chain (innermost-first internally) outermost-first
||| with ` <- ` separators — readable even when keys contain slashes.
chainStr : List String -> String
chainStr []        = ""
chainStr [x]       = x
chainStr (x :: xs) = chainStr xs ++ " <- " ++ x

public export
Show IncludeError where
  show (MissingInclude r c) = "include not found: " ++ r
                                ++ (if null c then "" else " (from " ++ chainStr c ++ ")")
  show (IncludeCycle r c)   = "include cycle at: " ++ r
                                ++ " (chain " ++ chainStr c ++ ")"
  show (DepthExceeded r c)  = "include depth exceeded at: " ++ r
                                ++ " (chain " ++ chainStr c ++ ")"

||| Replace each `{% include: PATH %}` line with the fully spliced
||| contents of PATH. `fuel` bounds recursion depth (totality + runaway
||| cap); `visited` is the ancestor chain of canonical keys (cycle
||| detection); `curDir` is the directory the current source's includes
||| resolve against. Non-directive lines pass through verbatim.
public export
spliceWith : ResolveFn -> (fuel : Nat) -> (visited : List String)
          -> (curDir : String) -> (source : String)
          -> Either IncludeError String
spliceWith resolve fuel visited curDir source =
  map joinNL (traverse go (lines source))
  where
    go : String -> Either IncludeError String
    go line = case parseIncludeDirective line of
      Nothing  => Right line
      Just ref => case fuel of
        Z   => Left (DepthExceeded ref visited)
        S k => case resolve curDir ref of
          Nothing => Left (MissingInclude ref visited)
          Just r  => if elem r.key visited
                       then Left (IncludeCycle r.key visited)
                       else spliceWith resolve k (r.key :: visited)
                                       r.baseDir r.contents
