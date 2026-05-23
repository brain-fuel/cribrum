||| MVP-TEAWeb demo — Counter + Focus button.
|||
||| The architectural keystone for TEAWeb. ~80 LOC of app code.
||| Demonstrates:
|||   - `HExpr`-as-view-return-type (via `View msg`)
|||   - Typed `onClick` carrying `msg`
|||   - Dispatch through the handler table installed by `mount`
|||   - `update` returning `(model, Cmd msg)`
|||   - A `Cmd` leaf (`Focus`) firing into the DOM
|||
||| Build:
|||   $ idris2 --cg javascript --build counter.ipkg
|||
||| Then open `index.html` in a browser — the bundle loads on
||| `window.onload` and mounts under `#app`.
module Main

import TEAWeb.Html
import TEAWeb.Event
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Program
import TEAWeb.Runtime

%default total

--------------------------------------------------------------------------------
-- Msg + Model.
--------------------------------------------------------------------------------

data Msg = Increment | Decrement | FocusInput

record Model where
  constructor MkModel
  count : Int

--------------------------------------------------------------------------------
-- TEA functions.
--------------------------------------------------------------------------------

init_ : (Model, Cmd Msg)
init_ = (MkModel 0, None)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ Increment  m = ({ count := m.count + 1 } m, None)
update_ Decrement  m = ({ count := m.count - 1 } m, None)
update_ FocusInput m = (m, Focus "name-input")

view_ : Model -> View Msg
view_ m =
  main_ [class_ "counter-app"]
    [ h1_ [] [ text_ "TEAWeb Counter" ]
    , p_  [class_ "value"] [ text_ ("Count: " ++ show m.count) ]
    , div_ [class_ "buttons"]
        [ button_ [ id_ "inc-btn", onClick "inc" Increment ]
            [ text_ "+" ]
        , button_ [ id_ "dec-btn", onClick "dec" Decrement ]
            [ text_ "-" ]
        ]
    , input_
        [ id_ "name-input"
        , type_ "text"
        , placeholder_ "type something here"
        ]
        []
    , button_ [ id_ "focus-btn", onClick "focus" FocusInput ]
        [ text_ "Focus the input" ]
    ]

subs_ : Model -> Sub Msg
subs_ _ = None

prog : Program Model Msg
prog = MkProgram init_ update_ view_ subs_

--------------------------------------------------------------------------------
-- Entry point. Mount under the DOM element with id "app".
--------------------------------------------------------------------------------

main : IO ()
main = mount prog "app"
