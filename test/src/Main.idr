module Main

import Hedgehog
import Test.Cribrum.Node
import Test.Cribrum.Djot.Surface
import Test.Cribrum.Djot.Parser
import Test.Cribrum.Html.Valid
import Test.Cribrum.Elaborate
import Test.Cribrum.Render.Html
import Test.Cribrum.Render.Dom
import Test.Cribrum.AA.Pass
import Test.Cribrum.AA.Typed
import Test.Cribrum.Integration

main : IO ()
main = test
  [ Test.Cribrum.Node.group
  , Test.Cribrum.Djot.Surface.group
  , Test.Cribrum.Djot.Parser.group
  , Test.Cribrum.Html.Valid.group
  , Test.Cribrum.Elaborate.group
  , Test.Cribrum.Render.Html.group
  , Test.Cribrum.Render.Dom.group
  , Test.Cribrum.AA.Pass.group
  , Test.Cribrum.AA.Typed.group
  , Test.Cribrum.Integration.group
  ]
