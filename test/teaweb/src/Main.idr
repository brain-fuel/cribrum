module Main

import Hedgehog
import Test.TEAWeb.Html
import Test.TEAWeb.Event

main : IO ()
main = test
  [ Test.TEAWeb.Html.group
  , Test.TEAWeb.Event.group
  ]
