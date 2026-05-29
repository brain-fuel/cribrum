module Main

import System
import System.File
import Hedgehog
import Test.Cribrum.Node
import Test.Cribrum.Djot.Surface
import Test.Cribrum.Djot.Parser
import Test.Cribrum.Html.Model
import Test.Cribrum.Html.Valid
import Test.Cribrum.Elaborate
import Test.Cribrum.Render.Html
import Test.Cribrum.Render.Dom
import Test.Cribrum.AA.Pass
import Test.Cribrum.AA.Typed
import Test.Cribrum.AA.Partition
import Test.Cribrum.Pipeline.Anchor
import Test.Cribrum.Preprocess
import Test.Cribrum.Integration

||| Read a file relative to the repo root (the cwd when `pack test
||| cribrum_test` or `make test-cribrum` invokes us). Abort the
||| process with a clear message on read failure — the integration
||| suite would otherwise pass vacuously on a missing fixture.
covering
readDoc : String -> IO String
readDoc path = do
  Right s <- readFile path
    | Left err => do
        putStrLn ("integration: cannot read " ++ path ++ ": " ++ show err)
        exitFailure
  pure s

covering
main : IO ()
main = do
  readmeSrc <- readDoc "README.dj"
  planSrc   <- readDoc "plan.dj"
  test
    [ Test.Cribrum.Node.group
    , Test.Cribrum.Djot.Surface.group
    , Test.Cribrum.Djot.Parser.group
    , Test.Cribrum.Html.Model.group
    , Test.Cribrum.Html.Valid.group
    , Test.Cribrum.Elaborate.group
    , Test.Cribrum.Render.Html.group
    , Test.Cribrum.Render.Dom.group
    , Test.Cribrum.AA.Pass.group
    , Test.Cribrum.AA.Typed.group
    , Test.Cribrum.AA.Partition.group
    , Test.Cribrum.Pipeline.Anchor.group
    , Test.Cribrum.Preprocess.group
    , Test.Cribrum.Integration.mkGroup readmeSrc planSrc
    ]
