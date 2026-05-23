||| EXT + PBT for `TEAWeb.Event`.
|||
||| The Event helpers are thin wrappers that compose into the
||| `TEAWeb.Html.Attr` data constructor `On`. Tests pin the wire format
||| (data-on-<event>="<callbackId>") so a name drift breaks here, not
||| at the JS dispatch boundary.
module Test.TEAWeb.Event

import Hedgehog
import Cribrum.Node
import TEAWeb.Html
import TEAWeb.Event

%default total

data Msg = MA | MB | MFromString String

Eq Msg where
  MA == MA = True
  MB == MB = True
  MFromString a == MFromString b = a == b
  _  == _  = False

Show Msg where
  show MA = "MA"
  show MB = "MB"
  show (MFromString s) = "MFromString " ++ show s

-- Helper: turn an Attr into the underlying HAttr (mirrors the private
-- attrToHAttr in TEAWeb.Html via a tiny element that strips one attr).
attrHAttr : Attr Msg -> HAttr
attrHAttr a = case tree (div_ [a] []) of
  Element _ (h :: _) _ => h
  _                    => MkHAttr "?" (Str "?")

export
ext_onClick_wires_click : Property
ext_onClick_wires_click = withTests 1 . property $ do
  attrHAttr (onClick "cb-1" MA) === MkHAttr "data-on-click" (Handler "click" "cb-1")

export
ext_onSubmit_wires_submit : Property
ext_onSubmit_wires_submit = withTests 1 . property $ do
  attrHAttr (onSubmit "cb-2" MA) === MkHAttr "data-on-submit" (Handler "submit" "cb-2")

export
ext_onFocus_wires_focus : Property
ext_onFocus_wires_focus = withTests 1 . property $ do
  attrHAttr (onFocus "cb-3" MA) === MkHAttr "data-on-focus" (Handler "focus" "cb-3")

export
ext_onBlur_wires_blur : Property
ext_onBlur_wires_blur = withTests 1 . property $ do
  attrHAttr (onBlur "cb-4" MA) === MkHAttr "data-on-blur" (Handler "blur" "cb-4")

export
ext_onDoubleClick_wires_dblclick : Property
ext_onDoubleClick_wires_dblclick = withTests 1 . property $ do
  attrHAttr (onDoubleClick "cb-5" MA) === MkHAttr "data-on-dblclick" (Handler "dblclick" "cb-5")

export
ext_onMouseEnter_wires_mouseenter : Property
ext_onMouseEnter_wires_mouseenter = withTests 1 . property $ do
  attrHAttr (onMouseEnter "cb-6" MA) === MkHAttr "data-on-mouseenter" (Handler "mouseenter" "cb-6")

export
ext_onMouseLeave_wires_mouseleave : Property
ext_onMouseLeave_wires_mouseleave = withTests 1 . property $ do
  attrHAttr (onMouseLeave "cb-7" MA) === MkHAttr "data-on-mouseleave" (Handler "mouseleave" "cb-7")

export
ext_onInput_wires_input : Property
ext_onInput_wires_input = withTests 1 . property $ do
  attrHAttr (onInput "cb-i" MFromString) === MkHAttr "data-on-input" (Handler "input" "cb-i")

export
ext_onChange_wires_change : Property
ext_onChange_wires_change = withTests 1 . property $ do
  attrHAttr (onChange "cb-c" MFromString) === MkHAttr "data-on-change" (Handler "change" "cb-c")

--------------------------------------------------------------------------------
-- PBT: handler closure ignores its event payload (msg form).
--------------------------------------------------------------------------------

dummyEvent : Event
dummyEvent = believe_me ()

export
pbt_onClick_closure_emits_msg : Property
pbt_onClick_closure_emits_msg = property $ do
  cb <- forAll $ Gen.string (Range.linear 1 5) Gen.alphaNum
  let v : View Msg
      v = button_ [onClick cb MA] []
  case handlers v of
    [(_, fn)] => fn dummyEvent === MA
    _         => failWith Nothing "expected one handler"

export
group : Group
group = MkGroup "TEAWeb.Event"
  [ ("ext_onClick_wires_click",           ext_onClick_wires_click)
  , ("ext_onSubmit_wires_submit",         ext_onSubmit_wires_submit)
  , ("ext_onFocus_wires_focus",           ext_onFocus_wires_focus)
  , ("ext_onBlur_wires_blur",             ext_onBlur_wires_blur)
  , ("ext_onDoubleClick_wires_dblclick",  ext_onDoubleClick_wires_dblclick)
  , ("ext_onMouseEnter_wires_mouseenter", ext_onMouseEnter_wires_mouseenter)
  , ("ext_onMouseLeave_wires_mouseleave", ext_onMouseLeave_wires_mouseleave)
  , ("ext_onInput_wires_input",           ext_onInput_wires_input)
  , ("ext_onChange_wires_change",         ext_onChange_wires_change)
  , ("pbt_onClick_closure_emits_msg",     pbt_onClick_closure_emits_msg)
  ]
