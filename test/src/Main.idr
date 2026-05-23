module Main

import Hedgehog
import Test.Cribrum.Node
import Test.Cribrum.Djot.Surface

main : IO ()
main = test
  [ Test.Cribrum.Node.group
  , Test.Cribrum.Djot.Surface.group
  ]
