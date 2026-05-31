||| TEAWeb leaf-coverage demo — every non-MVP Cmd / Sub leaf, end-to-end.
|||
||| The counter demo proves `Every` + `After` + `Focus`. This one closes
||| the loop on the rest of the inventory, so each FFI shim in
||| `Cribrum.Render.Dom` is exercised by a real bundle in a real browser
||| (the only place chez-untestable effects can be observed):
|||
|||   - `Cmd.Http`            — Fetch button issues `GET ./hello.json`;
|||                             the settle round-trips an `HttpResult`
|||                             back through the dispatch loop.
|||   - `Cmd.Random`          — Roll button draws a uniform die face.
|||   - `Cmd.SendPort` +      — Ping button sends on the `echo-out`
|||     `Sub.Port`              outbound port; the host page echoes it
|||                             back on the `echo` inbound port, which a
|||                             `Sub.Port` subscription folds into a msg.
|||   - `Sub.OnAnimationFrame` — a toggle adds/removes the rAF leaf, so
|||                             the subscription diff installs / tears it
|||                             down live (watch the frame counter run
|||                             only while enabled).
|||   - `Sub.OnKeyDown`       — a document-level key listener shows the
|||                             last key pressed anywhere on the page.
|||
||| Build:
|||   $ pack --cg javascript build leafdemo.ipkg
|||
||| Then serve this directory over HTTP (the `fetch` needs a real
||| origin — `file://` blocks it) and open `index.html`:
|||   $ python3 -m http.server -d examples/teaweb/leafdemo
module Main

import Data.String
import TEAWeb.Html
import TEAWeb.Event
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Ports
import TEAWeb.Program
import TEAWeb.Runtime

%default total

--------------------------------------------------------------------------------
-- Msg + Model.
--------------------------------------------------------------------------------

data Msg
  = Fetch                 -- Cmd.Http   : issue GET ./hello.json
  | GotHttp HttpResult    -- Cmd.Http   : the request settled
  | Roll                  -- Cmd.Random : draw a die face
  | GotRoll Integer       -- Cmd.Random : the value came back
  | ToggleRaf             -- Sub.OnAnimationFrame : add / drop the leaf
  | RafTick Double        -- Sub.OnAnimationFrame : one frame fired
  | KeyPressed String     -- Sub.OnKeyDown : a key went down on the page
  | Ping                  -- Cmd.SendPort : push out the echo port
  | GotEcho String        -- Sub.Port     : the host echoed back

record Model where
  constructor MkModel
  httpState  : String     -- last fetch outcome, human-readable
  dice       : String     -- last die face, or "—"
  rafRunning : Bool        -- is the animation-frame leaf installed?
  rafTicks   : Int         -- frames counted since the leaf was (re)enabled
  lastKey    : String     -- most recent keydown anywhere
  sends      : Int         -- how many pings we've emitted
  echo       : String     -- last payload the host echoed back

--------------------------------------------------------------------------------
-- TEA functions.
--------------------------------------------------------------------------------

init_ : (Model, Cmd Msg)
init_ = (MkModel "(not fetched)" "—" False 0 "(none)" 0 "(none)", None)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ Fetch m =
  (m, Http GET "./hello.json" [] "" GotHttp)
update_ (GotHttp (HttpOk status body)) m =
  ({ httpState := "HTTP " ++ show status ++ " — " ++ body } m, None)
update_ (GotHttp (HttpErr reason)) m =
  ({ httpState := "error: " ++ reason } m, None)
update_ Roll m =
  (m, Random 1 6 GotRoll)
update_ (GotRoll n) m =
  ({ dice := show n } m, None)
update_ ToggleRaf m =
  ({ rafRunning $= not, rafTicks := 0 } m, None)
update_ (RafTick _) m =
  ({ rafTicks $= (+ 1) } m, None)
update_ (KeyPressed k) m =
  ({ lastKey := k } m, None)
update_ Ping m =
  let n = m.sends + 1
   in ({ sends := n } m, send "echo-out" ("ping #" ++ show n))
update_ (GotEcho s) m =
  ({ echo := s } m, None)

--------------------------------------------------------------------------------
-- View.
--------------------------------------------------------------------------------

row : String -> String -> View Msg
row label val =
  p_ [class_ "row"]
    [ span_ [class_ "label"] [ text_ label ]
    , span_ [class_ "val"]   [ text_ val ]
    ]

view_ : Model -> View Msg
view_ m =
  main_ [class_ "leaf-demo"]
    [ h1_ [] [ text_ "TEAWeb — leaf coverage" ]

    , section_ []
        [ h2_ [] [ text_ "Cmd.Http" ]
        , button_ [ id_ "fetch-btn", onClick "fetch" Fetch ] [ text_ "Fetch ./hello.json" ]
        , row "result:" m.httpState
        ]

    , section_ []
        [ h2_ [] [ text_ "Cmd.Random" ]
        , button_ [ id_ "roll-btn", onClick "roll" Roll ] [ text_ "Roll a die" ]
        , row "face:" m.dice
        ]

    , section_ []
        [ h2_ [] [ text_ "Sub.OnAnimationFrame" ]
        , button_ [ id_ "raf-btn", onClick "raf" ToggleRaf ]
            [ text_ (if m.rafRunning then "Stop frames" else "Start frames") ]
        , row "frames:" (show m.rafTicks)
        ]

    , section_ []
        [ h2_ [] [ text_ "Sub.OnKeyDown" ]
        , p_ [] [ text_ "Press any key (the page listens at the document level)." ]
        , row "last key:" m.lastKey
        ]

    , section_ []
        [ h2_ [] [ text_ "Cmd.SendPort + Sub.Port" ]
        , button_ [ id_ "ping-btn", onClick "ping" Ping ] [ text_ "Ping the echo port" ]
        , row "sent:"  (show m.sends)
        , row "echo:"  m.echo
        ]
    ]

--------------------------------------------------------------------------------
-- Subscriptions. `Batch` of always-on leaves plus the rAF leaf that
-- appears only while enabled — so toggling it drives the runtime's
-- subscription diff (install on enable, teardown on disable).
--------------------------------------------------------------------------------

subs_ : Model -> Sub Msg
subs_ m =
  Batch
    [ OnKeyDown "keydown" KeyPressed
    , subscribeNamed "echo" GotEcho        -- Port "port:echo" "echo" GotEcho
    , if m.rafRunning
        then OnAnimationFrame "raf-frames" RafTick
        else None
    ]

prog : Program Model Msg
prog = MkProgram init_ update_ view_ subs_

main : IO ()
main = mount prog "app"
