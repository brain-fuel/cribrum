module Main

import Hedgehog
import Test.TEAWeb.Html
import Test.TEAWeb.Event
import Test.TEAWeb.Cmd
import Test.TEAWeb.Program

main : IO ()
main = test
  [ Test.TEAWeb.Html.group
  , Test.TEAWeb.Event.group
  , Test.TEAWeb.Cmd.group
  , Test.TEAWeb.Program.group
  ]
