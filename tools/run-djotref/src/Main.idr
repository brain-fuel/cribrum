||| `run-djotref` — the Djot reference-suite gate.
|||
||| Walks `test/djot-ref/corpus/*.test`, runs each through Cribrum's
||| `parseDoc -> elaborate -> renderHtml` pipeline, and compares the
||| produced HTML against the expected HTML embedded in the test file.
|||
||| Test file format (one test per file):
|||
|||     === input ===
|||     <djot source>
|||     === expected ===
|||     <reference HTML>
|||
||| The expected HTML is what an *upstream-reference* Djot renderer
||| produces. Cribrum's output is normalised before comparison
||| (whitespace collapsed, surrounding `<main>` landmark stripped)
||| since Cribrum wraps every document in `<main>` and emits without
||| inter-element newlines.
|||
||| Baseline gate: `test/djot-ref/baseline.txt` lists the test names
||| that currently pass. The exit code is 0 only when every baselined
||| test still passes; a previously-passing test failing is a hard
||| regression. Tests not in the baseline are reported but do not
||| affect the exit code, so adding a failing test to the corpus is
||| safe (the next parser slice can move it into the baseline).
|||
||| Usage:
|||
|||     run-djotref                  # report + gate against baseline
|||     run-djotref --update         # refresh baseline.txt with the
|||                                  # current pass set (use after a
|||                                  # parser slice lands)
module Main

import System
import System.File
import System.Directory
import Data.List
import Data.String
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser
import Cribrum.Html.Valid
import Cribrum.Elaborate
import Cribrum.Render.Html
import Cribrum.Pipeline.Anchor

%default total

--------------------------------------------------------------------------------
-- Test discovery + parse.
--------------------------------------------------------------------------------

record TestCase where
  constructor MkTC
  name     : String
  input    : String
  expected : String

covering
readDir : String -> IO (List String)
readDir path = do
  Right entries <- listDir path
    | Left err => do
        putStrLn ("listDir error (" ++ path ++ "): " ++ show err)
        pure []
  pure entries

covering
readFileOrEmpty : String -> IO String
readFileOrEmpty path = do
  Right s <- readFile path
    | Left _ => pure ""
  pure s

||| Drop a known suffix from a string, returning the prefix.
dropSuffix : (suffix : String) -> String -> String
dropSuffix suf s =
  if isSuffixOf suf s
    then substr 0 (length s `minus` length suf) s
    else s

||| Split a test source into `(input, expected)` by scanning for the
||| literal marker lines `=== input ===` and `=== expected ===`.
||| Anything before `=== input ===` is ignored. Both sections are
||| trimmed of a single trailing newline.
splitTestSrc : String -> Maybe (String, String)
splitTestSrc src =
  let ls = lines src
   in case break (== "=== input ===") ls of
        (_, _ :: afterInput) =>
          case break (== "=== expected ===") afterInput of
            (inputLines, _ :: expectedLines) =>
              Just ( unlines inputLines
                   , unlines expectedLines )
            _ => Nothing
        _ => Nothing

covering
loadTest : (dir : String) -> (file : String) -> IO (Maybe TestCase)
loadTest dir file =
  if not (isSuffixOf ".test" file)
    then pure Nothing
    else do
      src <- readFileOrEmpty (dir ++ "/" ++ file)
      case splitTestSrc src of
        Just (i, e) =>
          pure (Just (MkTC (dropSuffix ".test" file) i e))
        Nothing => do
          putStrLn ("skipping " ++ file
                     ++ ": missing `=== input ===` / `=== expected ===` markers")
          pure Nothing

covering
loadAllTests : String -> IO (List TestCase)
loadAllTests dir = do
  entries <- readDir dir
  cases <- traverse (loadTest dir) (sort entries)
  pure (mapMaybe id cases)

--------------------------------------------------------------------------------
-- Pipeline + comparison.
--------------------------------------------------------------------------------

||| Strip the outer `<main>` landmark Cribrum wraps every document in,
||| so the body can be compared against a reference renderer that
||| doesn't add a landmark. If the output isn't wrapped (empty doc),
||| returns the input untouched.
stripMain : String -> String
stripMain s =
  let stripped = if isPrefixOf "<main>" s && isSuffixOf "</main>" s
                   then substr 6 (length s `minus` 13) s
                   else s
   in stripped

||| Normalise whitespace: collapse any run of whitespace (spaces,
||| tabs, newlines) into a single space, then trim, then drop any
||| whitespace adjacent to a tag boundary (`> ` -> `>` and ` <` -> `<`).
||| Cribrum's renderer emits zero whitespace between tags; the
||| reference renderer emits one element per line. After this
||| normalisation, both shapes collapse to the same string.
||| Whitespace inside textual content stays a single space, so
||| `<p>hello world</p>` round-trips, but inter-tag indentation is
||| ignored.
normWhitespace : String -> String
normWhitespace = pack . dropTagSpace . trimDup [] . unpack
  where
    -- Walk char-by-char; emit one space per run of whitespace.
    trimDup : List Char -> List Char -> List Char
    trimDup acc [] = reverse (dropWhile isSpace (dropWhile isSpace acc))
    trimDup acc (c :: cs) =
      if isSpace c
        then case acc of
          (' ' :: _) => trimDup acc cs
          _          => trimDup (' ' :: acc) cs
        else trimDup (c :: acc) cs

    -- After `trimDup`, every whitespace run is exactly one space.
    -- Drop the space when it sits adjacent to a tag boundary:
    --   `> `  -> `>`  (after a closing/opening tag delimiter)
    --   ` <`  -> `<`  (before an opening tag delimiter)
    -- This means a reference renderer's `<p>foo</p>\n<p>bar</p>`
    -- and Cribrum's `<p>foo</p><p>bar</p>` collapse to the same
    -- string, and `<li>\nfoo\n</li>` matches `<li>foo</li>`.
    -- Whitespace inside text without adjacent tag chars stays put
    -- (`<p>hello world</p>` round-trips).
    dropTagSpace : List Char -> List Char
    dropTagSpace []                = []
    dropTagSpace ('>' :: ' ' :: cs) = '>' :: dropTagSpace cs
    dropTagSpace (' ' :: '<' :: cs) = '<' :: dropTagSpace cs
    dropTagSpace (c :: cs)          = c   :: dropTagSpace cs

||| Run a TestCase through the pipeline. Returns `Right body` on
||| success, `Left reason` on parse/elaborate failure. `addSectionIds`
||| fills in the heading-derived section anchors the reference renderer
||| also emits (elaborate leaves them unset; see `Cribrum.Pipeline.Anchor`).
runPipeline : String -> Either String String
runPipeline src = case parseDoc src of
  Left e  => Left ("parse failed: " ++ show e)
  Right d => case elaborate d of
    Left e             => Left ("elaborate failed: " ++ show e)
    Right (h ** (_, _)) => Right (renderHtml (addSectionIds h))

data Outcome = Pass | Fail String | Error String

passName : (String, Outcome) -> Maybe String
passName (n, Pass) = Just n
passName _         = Nothing

regressionMissing : List String -> List String -> List String
regressionMissing baseline nowPassing =
  filter (\n => not (elem n nowPassing)) baseline

evalCase : TestCase -> Outcome
evalCase tc = case runPipeline (input tc) of
  Left e    => Error e
  Right out =>
    let actual   = normWhitespace (stripMain out)
        expected = normWhitespace (expected tc)
     in if actual == expected
          then Pass
          else Fail ("expected: " ++ expected ++ "\n  actual: " ++ actual)

--------------------------------------------------------------------------------
-- Baseline gate.
--------------------------------------------------------------------------------

||| Load the baseline file as a set of test names that should pass.
covering
loadBaseline : String -> IO (List String)
loadBaseline path = do
  src <- readFileOrEmpty path
  pure $ filter (\l => l /= "") (map trim (lines src))

||| Sort + dedupe a list of strings.
sortedUnique : List String -> List String
sortedUnique = dedup . sort
  where
    dedup : List String -> List String
    dedup []        = []
    dedup [x]       = [x]
    dedup (x :: xs@(y :: _)) =
      if x == y then dedup xs else x :: dedup xs

covering
writeBaseline : String -> List String -> IO ()
writeBaseline path names = do
  let body = unlines (sortedUnique names)
  Right () <- writeFile path body
    | Left err => do
        putStrLn ("write error (" ++ path ++ "): " ++ show err)
        exitFailure
  putStrLn ("baseline updated: " ++ path
             ++ " (" ++ show (length names) ++ " passing tests)")

--------------------------------------------------------------------------------
-- Main.
--------------------------------------------------------------------------------

corpusDir : String
corpusDir = "test/djot-ref/corpus"

baselinePath : String
baselinePath = "test/djot-ref/baseline.txt"

covering
main : IO ()
main = do
  args  <- getArgs
  let updateMode = elem "--update" args
  cases <- loadAllTests corpusDir
  let outcomes = map (\c => (name c, evalCase c)) cases
  baseline <- loadBaseline baselinePath
  let baselineSet = sortedUnique baseline

  putStrLn ("=== djot-ref corpus: " ++ show (length cases) ++ " tests ===")
  for_ outcomes $ \(n, o) =>
    case o of
      Pass     => putStrLn ("  PASS  " ++ n)
      Fail msg => putStrLn ("  FAIL  " ++ n ++ "  " ++ msg)
      Error e  => putStrLn ("  ERR   " ++ n ++ "  " ++ e)
  let passingNow = mapMaybe passName outcomes
  let totalTests = length cases
  putStrLn ""
  putStrLn ("passed: " ++ show (length passingNow) ++ " / " ++ show totalTests)
  putStrLn ("baseline: " ++ show (length baselineSet) ++ " test(s)")

  if updateMode
    then writeBaseline baselinePath passingNow
    else do
      -- Regression check: every baseline name must still pass.
      let nowPassingSet = sortedUnique passingNow
      let regressed     = regressionMissing baselineSet nowPassingSet
      case regressed of
        [] => do
          putStrLn "no baseline regressions"
          exitSuccess
        rs => do
          putStrLn ""
          putStrLn "BASELINE REGRESSIONS:"
          for_ rs $ \n => putStrLn ("  - " ++ n)
          exitFailure
