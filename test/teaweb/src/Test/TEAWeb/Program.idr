||| Pure-Idris tests for `TEAWeb.Program`'s `update` + `view` pipeline.
|||
||| The Program record is just a container; we can still exercise the
||| math of a tiny app's update/view without ever touching a DOM. This
||| is what guards against regressions in `Cmd` shape, `View msg`
||| structure, and handler-table propagation.
module Test.TEAWeb.Program

import Hedgehog
import Cribrum.Node
import TEAWeb.Html
import TEAWeb.Event
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Program

%default total

--------------------------------------------------------------------------------
-- Tiny counter app — same shape as the MVP-TEAWeb demo.
--------------------------------------------------------------------------------

data Msg = Increment | Decrement | FocusInput

Eq Msg where
  Increment  == Increment  = True
  Decrement  == Decrement  = True
  FocusInput == FocusInput = True
  _          == _          = False

Show Msg where
  show Increment  = "Increment"
  show Decrement  = "Decrement"
  show FocusInput = "FocusInput"

record CounterModel where
  constructor MkCounter
  count : Int

initModel : CounterModel
initModel = MkCounter 0

initial : (CounterModel, Cmd Msg)
initial = (initModel, None)

update_ : Msg -> CounterModel -> (CounterModel, Cmd Msg)
update_ Increment  m = ({count := count m + 1} m, None)
update_ Decrement  m = ({count := count m - 1} m, None)
update_ FocusInput m = (m, Focus "name-input")

view_ : CounterModel -> View Msg
view_ m =
  div_ [class_ "app"]
    [ h1_ [] [text_ ("Count: " ++ show m.count)]
    , button_ [id_ "inc-btn", onClick "inc" Increment] [text_ "+"]
    , button_ [id_ "dec-btn", onClick "dec" Decrement] [text_ "-"]
    , input_  [id_ "name-input", type_ "text", placeholder_ "type here"] []
    , button_ [id_ "focus-btn", onClick "focus" FocusInput] [text_ "Focus input"]
    ]

subs_ : CounterModel -> Sub Msg
subs_ _ = None

counterProgram : Program CounterModel Msg
counterProgram = MkProgram
  initial
  update_
  view_
  subs_

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_init_starts_at_zero : Property
ext_init_starts_at_zero = withTests 1 . property $ do
  let (m, _) = init counterProgram
  count m === 0

export
ext_init_cmd_is_none : Property
ext_init_cmd_is_none = withTests 1 . property $ do
  let (_, c) = init counterProgram
  length (flatten c) === 0

export
ext_increment_bumps_count : Property
ext_increment_bumps_count = withTests 1 . property $ do
  let (m1, _) = update counterProgram Increment (MkCounter 5)
  count m1 === 6

export
ext_decrement_lowers_count : Property
ext_decrement_lowers_count = withTests 1 . property $ do
  let (m1, _) = update counterProgram Decrement (MkCounter 5)
  count m1 === 4

export
ext_focus_emits_focus_cmd : Property
ext_focus_emits_focus_cmd = withTests 1 . property $ do
  let (_, c) = update counterProgram FocusInput (MkCounter 0)
  case c of
    Focus id => id === "name-input"
    other    => failWith Nothing ("expected Focus, got " ++ show (length (flatten other)))

export
ext_view_emits_main_structure : Property
ext_view_emits_main_structure = withTests 1 . property $ do
  let v = view counterProgram (MkCounter 7)
  case tree v of
    Element t _ children => do
      t === "div"
      length children === 5
    _ => failWith Nothing "expected Element"

export
ext_view_has_three_click_handlers : Property
ext_view_has_three_click_handlers = withTests 1 . property $ do
  let v = view counterProgram (MkCounter 0)
  length (handlers v) === 3
  map fst (handlers v) === ["inc", "dec", "focus"]

export
ext_subscriptions_are_none : Property
ext_subscriptions_are_none = withTests 1 . property $ do
  let s = subscriptions counterProgram (MkCounter 0)
  case s of
    None => success
    _    => failWith Nothing "expected None"

--------------------------------------------------------------------------------
-- PBT — incrementing N times leaves count = init + N.
--------------------------------------------------------------------------------

repeatUpdate : (msg : Msg) -> Nat -> CounterModel -> CounterModel
repeatUpdate _   Z      m = m
repeatUpdate msg (S k)  m = repeatUpdate msg k (fst (update counterProgram msg m))

export
pbt_increment_is_additive : Property
pbt_increment_is_additive = property $ do
  n <- forAll $ Gen.nat (Range.linear 0 50)
  let m = repeatUpdate Increment n (MkCounter 0)
  count m === cast n

export
group : Group
group = MkGroup "TEAWeb.Program"
  [ ("ext_init_starts_at_zero",          ext_init_starts_at_zero)
  , ("ext_init_cmd_is_none",             ext_init_cmd_is_none)
  , ("ext_increment_bumps_count",        ext_increment_bumps_count)
  , ("ext_decrement_lowers_count",       ext_decrement_lowers_count)
  , ("ext_focus_emits_focus_cmd",        ext_focus_emits_focus_cmd)
  , ("ext_view_emits_main_structure",    ext_view_emits_main_structure)
  , ("ext_view_has_three_click_handlers", ext_view_has_three_click_handlers)
  , ("ext_subscriptions_are_none",       ext_subscriptions_are_none)
  , ("pbt_increment_is_additive",        pbt_increment_is_additive)
  ]
