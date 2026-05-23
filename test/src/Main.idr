module Main

import Hedgehog
import Test.Cribrum.Node

main : IO ()
main = test
  [ Test.Cribrum.Node.group
  ]
