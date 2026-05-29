||| `preprocess` — resolve `{% include: path %}` directives in a Djot
||| source file, emitting one combined `.dj` document.
|||
||| Include paths are relative to the *including* file's directory. The
||| combined output is meant to feed straight into `parseDoc` (e.g. via
||| `render-doc`), so the whole document parses + elaborates as one — and
||| therefore carries exactly one `<main>` (no nested-`<main>` risk).
|||
||| All recognition / path / splice logic is the pure, tested core in
||| `Cribrum.Preprocess`; this tool is the thin IO shell: it BFS-loads the
||| transitive closure of referenced files, then runs the pure splicer.
|||
||| Usage:
|||
|||   preprocess <input.dj> [output.dj]   # no output ⇒ write to stdout
module Main

import System
import System.File
import Data.List
import Data.String
import Cribrum.Preprocess

%default covering

||| Read a file, `Nothing` on any error.
tryRead : String -> IO (Maybe String)
tryRead path = do
  Right s <- readFile path
    | Left _ => pure Nothing
  pure (Just s)

writeOrFail : String -> String -> IO ()
writeOrFail path contents = do
  Right () <- writeFile path contents
    | Left err => do
        putStrLn ("write error (" ++ path ++ "): " ++ show err)
        exitFailure
  pure ()

||| The include references appearing in a source (curDir-relative).
refsOf : String -> List String
refsOf src = mapMaybe parseIncludeDirective (lines src)

||| BFS-load every file reachable through includes into a canonical
||| `(key -> contents)` map. Already-loaded keys are skipped, so cycles
||| terminate here (the *pure* splicer is what reports them as errors).
||| Unreadable references are left out; the splicer then reports them as
||| `MissingInclude`.
loadClosure : List (String, String) -> List (String, String)
           -> IO (List (String, String))
loadClosure acc []                       = pure acc
loadClosure acc ((curDir, ref) :: queue) =
  let key = resolvePath curDir ref in
  case lookup key acc of
    Just _  => loadClosure acc queue
    Nothing => do
      Just contents <- tryRead key
        | Nothing => loadClosure acc queue
      let kids = map (\r => (dirname key, r)) (refsOf contents)
      loadClosure ((key, contents) :: acc) (kids ++ queue)

||| Pure resolver over the pre-loaded closure.
mkResolve : List (String, String) -> ResolveFn
mkResolve fs curDir ref =
  let key = resolvePath curDir ref in
  case lookup key fs of
    Just c  => Just (MkResolved key c (dirname key))
    Nothing => Nothing

main : IO ()
main = do
  args <- getArgs
  case args of
    (_ :: input :: outRest) => do
      Just src <- tryRead input
        | Nothing => do
            putStrLn ("read error (" ++ input ++ ")")
            exitFailure
      let entryDir = dirname input
      fs <- loadClosure [] (map (\r => (entryDir, r)) (refsOf src))
      case spliceWith (mkResolve fs) 64 [] entryDir src of
        Left e    => do
          putStrLn ("preprocess: " ++ show e)
          exitFailure
        Right out => case outRest of
          (output :: _) => do
            writeOrFail output out
            putStrLn ("wrote " ++ output ++ " (" ++ show (length out)
                       ++ " bytes)")
          []            => putStr out
    _ => do
      putStrLn "usage: preprocess <input.dj> [output.dj]"
      exitFailure
