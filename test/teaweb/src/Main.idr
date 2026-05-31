module Main

import Hedgehog
import Test.TEAWeb.Html
import Test.TEAWeb.HtmlTyped
import Test.TEAWeb.HtmlAccessible
import Test.TEAWeb.Event
import Test.TEAWeb.Cmd
import Test.TEAWeb.Sub
import Test.TEAWeb.SubDiff
import Test.TEAWeb.Ports
import Test.TEAWeb.Keyed
import Test.TEAWeb.AttrDiff
import Test.TEAWeb.Program

main : IO ()
main = test
  [ Test.TEAWeb.Html.group
  , Test.TEAWeb.HtmlTyped.group
  , Test.TEAWeb.HtmlAccessible.group
  , Test.TEAWeb.Event.group
  , Test.TEAWeb.Cmd.group
  , Test.TEAWeb.Sub.group
  , Test.TEAWeb.SubDiff.group
  , Test.TEAWeb.Ports.group
  , Test.TEAWeb.Keyed.group
  , Test.TEAWeb.AttrDiff.group
  , Test.TEAWeb.Program.group
  ]
