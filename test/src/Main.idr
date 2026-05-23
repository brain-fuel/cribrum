module Main

import Hedgehog
import Test.Cribrum.Node
import Test.Cribrum.Djot.Surface
import Test.Cribrum.Djot.Parser
import Test.Cribrum.Html.Valid
import Test.Cribrum.Elaborate

main : IO ()
main = test
  [ Test.Cribrum.Node.group
  , Test.Cribrum.Djot.Surface.group
  , Test.Cribrum.Djot.Parser.group
  , Test.Cribrum.Html.Valid.group
  , Test.Cribrum.Elaborate.group
  ]
