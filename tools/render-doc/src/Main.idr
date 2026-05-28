||| `render-doc` — drive the Cribrum pipeline from the command line.
|||
||| Reads a Djot source file, runs it through the *actual* Cribrum pipeline
|||
|||   parseDoc  (Cribrum.Djot.Parser)
|||      -> elaborate (Cribrum.Elaborate, strict)
|||      -> addSectionIds (Cribrum.Pipeline.Anchor)
|||      -> renderHtml (Cribrum.Render.Html)
|||
||| and writes the result wrapped in a minimal `<!doctype html>` shell so
||| the output is directly viewable in a browser. The elaboration step
||| produces a proof-carrying tree (`(h ** IsValidHtml h × StructuralAA h)`),
||| so a successful render is a witness that the source parsed, elaborated
||| to valid HTML, and passed every Structural AA rule from the catalog.
|||
||| Usage:
|||
|||   render-doc <input.dj> <output.html> <page-title>
module Main

import System
import System.File
import Data.String
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser
import Cribrum.Html.Valid
import Cribrum.Elaborate
import Cribrum.Render.Html
import Cribrum.Pipeline.Anchor

%default total

||| Minimal `<!doctype html>` wrapper around the rendered body. The body
||| itself already begins with `<main>` (the elaborator's default
||| landmark), so the wrapper provides only `<html>`/`<head>`/`<body>`.
wrapDocument : (title : String) -> (body : String) -> String
wrapDocument title body =
  "<!doctype html>\n"
    ++ "<html lang=\"en\">\n"
    ++ "<head>\n"
    ++ "  <meta charset=\"utf-8\">\n"
    ++ "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    ++ "  <title>" ++ escapeText title ++ "</title>\n"
    ++ "</head>\n"
    ++ "<body>\n"
    ++ body ++ "\n"
    ++ "</body>\n"
    ++ "</html>\n"

||| Run one source through parse + elaborate + render. Returns either a
||| human-readable error message (parse / elaborate failure) or the body
||| HTML produced by the renderer.
public export
renderSource : String -> Either String String
renderSource src = case parseDoc src of
  Left e  => Left ("parse failed: " ++ show e)
  Right d => case elaborate d of
    Left e             => Left ("elaborate failed: " ++ show e)
    Right (h ** (_, _)) => Right (renderHtml (addSectionIds h))

usage : IO ()
usage = do
  putStrLn "usage: render-doc <input.dj> <output.html> <page-title>"
  exitFailure

covering
readSourceOrFail : String -> IO String
readSourceOrFail path = do
  Right s <- readFile path
    | Left err => do
        putStrLn ("read error (" ++ path ++ "): " ++ show err)
        exitFailure
  pure s

covering
writeOrFail : String -> String -> IO ()
writeOrFail path contents = do
  Right () <- writeFile path contents
    | Left err => do
        putStrLn ("write error (" ++ path ++ "): " ++ show err)
        exitFailure
  pure ()

covering
main : IO ()
main = do
  args <- getArgs
  case args of
    [_, input, output, title] => do
      src <- readSourceOrFail input
      case renderSource src of
        Left err   => do
          putStrLn err
          exitFailure
        Right body => do
          writeOrFail output (wrapDocument title body)
          putStrLn ("wrote " ++ output ++ " (" ++ show (length body)
                     ++ " body bytes)")
    _ => usage
