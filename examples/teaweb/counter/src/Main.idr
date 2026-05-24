||| MVP-TEAWeb demo — Counter + word transforms.
|||
||| The architectural keystone for TEAWeb. Demonstrates:
|||   - `HExpr`-as-view-return-type (via `View msg`)
|||   - Typed `onClick` / `onInput` carrying `msg`
|||   - Dispatch through the handler table installed by `mount`
|||   - `update` returning `(model, Cmd msg)`
|||   - A `Cmd` leaf (`Focus`) firing into the DOM
|||   - Controlled input + Day-1.6 focus preservation across reconcile
|||   - Checkbox-driven view recomputation
|||
||| Build:
|||   $ idris2 --cg javascript --build counter.ipkg
|||
||| Then open `index.html` in a browser — the bundle loads on
||| `window.onload` and mounts under `#app`.
module Main

import Data.List
import Data.String
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

data Msg
  = Increment
  | Decrement
  | FocusInput
  | UpdateWord String
  | ClearWord
  | ToggleReverse
  | ToggleStripVowels
  | ToggleStripConsonants

record Model where
  constructor MkModel
  count           : Int
  word            : String
  reverseOn       : Bool
  stripVowels     : Bool
  stripConsonants : Bool

--------------------------------------------------------------------------------
-- Transform pipeline.
--------------------------------------------------------------------------------

isVowel : Char -> Bool
isVowel c = toLower c `elem` ['a', 'e', 'i', 'o', 'u']

isConsonant : Char -> Bool
isConsonant c = isAlpha c && not (isVowel c)

applyTransforms : Model -> String -> String
applyTransforms m s =
  let dropVowel = if m.stripVowels     then filter (not . isVowel)     else id
      dropCons  = if m.stripConsonants then filter (not . isConsonant) else id
      rev       = if m.reverseOn       then reverse                    else id
  in  pack (rev (dropCons (dropVowel (unpack s))))

--------------------------------------------------------------------------------
-- TEA functions.
--------------------------------------------------------------------------------

init_ : (Model, Cmd Msg)
init_ = (MkModel 0 "" False False False, None)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ Increment             m = ({ count           := m.count + 1 } m, None)
update_ Decrement             m = ({ count           := m.count - 1 } m, None)
update_ FocusInput            m = (m, Focus "word-input")
update_ (UpdateWord w)        m = ({ word            := w           } m, None)
update_ ClearWord             m = ({ word            := ""          } m, Focus "word-input")
update_ ToggleReverse         m = ({ reverseOn       $= not         } m, None)
update_ ToggleStripVowels     m = ({ stripVowels     $= not         } m, None)
update_ ToggleStripConsonants m = ({ stripConsonants $= not         } m, None)

--------------------------------------------------------------------------------
-- View.
--------------------------------------------------------------------------------

-- Conditionally include the `checked` attribute. Empty list when the
-- box is unchecked, single-element list when checked.
checkedIf : Bool -> List (Attr msg)
checkedIf True  = [checked_]
checkedIf False = []

transformCheckbox :
     (cbId : String)
  -> (msg : Msg)
  -> (checked : Bool)
  -> (label : String)
  -> View Msg
transformCheckbox cbId msg isChecked labelTxt =
  label_ [class_ "toggle"]
    [ input_
        ( [ type_ "checkbox"
          , id_ cbId
          , onClick cbId msg
          ] ++ checkedIf isChecked
        ) []
    , text_ (" " ++ labelTxt)
    ]

view_ : Model -> View Msg
view_ m =
  let transformed = applyTransforms m m.word
   in main_ [class_ "counter-app"]
        [ h1_ [] [ text_ "TEAWeb Counter" ]
        , p_  [class_ "value"] [ text_ ("Count: " ++ show m.count) ]
        , div_ [class_ "buttons"]
            [ button_ [ id_ "inc-btn", onClick "inc" Increment ] [ text_ "+" ]
            , button_ [ id_ "dec-btn", onClick "dec" Decrement ] [ text_ "-" ]
            ]
        , input_
            [ id_ "word-input"
            , type_ "text"
            , placeholder_ "type a word"
            , value_ m.word
            , onInput "word-input" UpdateWord
            ]
            []
        , div_ [class_ "actions"]
            [ button_ [ id_ "focus-btn", onClick "focus" FocusInput ]
                [ text_ "Focus the input" ]
            , button_ [ id_ "clear-btn", onClick "clear" ClearWord ]
                [ text_ "Clear" ]
            ]
        , div_ [class_ "transforms"]
            [ transformCheckbox "rev-cb"  ToggleReverse         m.reverseOn       "reverse"
            , transformCheckbox "vow-cb"  ToggleStripVowels     m.stripVowels     "filter out vowels"
            , transformCheckbox "cons-cb" ToggleStripConsonants m.stripConsonants "filter out consonants"
            ]
        , p_ [class_ "result"]
            [ text_ (if m.word == ""
                       then "(type a word to see it transformed)"
                       else transformed)
            ]
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
