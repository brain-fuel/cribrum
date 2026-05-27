module Main

import Hedgehog
import Test.TEAWeb.Html
import Test.TEAWeb.HtmlTyped
import Test.TEAWeb.Event
import Test.TEAWeb.Cmd
import Test.TEAWeb.Sub
import Test.TEAWeb.Program

main : IO ()
main = test
  [ Test.TEAWeb.Html.group
  , Test.TEAWeb.HtmlTyped.group
  , Test.TEAWeb.Event.group
  , Test.TEAWeb.Cmd.group
  , Test.TEAWeb.Sub.group
  , Test.TEAWeb.Program.group
  ]
