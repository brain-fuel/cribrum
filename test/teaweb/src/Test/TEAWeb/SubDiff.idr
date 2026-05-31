||| Pure-logic tests for subscription diffing (`TEAWeb.Sub.diffSubs` /
||| `subKey`).
|||
||| The install / teardown FFI is chez-untestable and lands in the
||| browser demo. What these tests pin is the pure decision core: given
||| the previously-installed leaf set and a freshly-evaluated one, which
||| leaves get installed and which callback ids get torn down. `Sub msg`
||| holds projection functions and has no `Eq`/`Show`, so installs are
||| asserted via `map subCallbackId` and teardowns are already a
||| `List String`.
module Test.TEAWeb.SubDiff

import TEAWeb.Sub
import Hedgehog
import Data.List

%default total

--------------------------------------------------------------------------------
-- Fixtures: a trivial msg type + projection helpers.
--------------------------------------------------------------------------------

data Msg = Tick Double | Key String | PortMsg String

key' : String -> Msg
key' = Key

tick' : Double -> Msg
tick' = Tick

port' : String -> Msg
port' = PortMsg

||| Install-set assertion view: the callback ids of the leaves to install.
installCbs : (List (Sub Msg), List String) -> List String
installCbs = mapMaybe subCallbackId . fst

||| Teardown-set assertion view: already a `List String` of callback ids.
teardownCbs : (List (Sub Msg), List String) -> List String
teardownCbs = snd

--------------------------------------------------------------------------------
-- subKey.
--------------------------------------------------------------------------------

ext_subkey_none : Property
ext_subkey_none = withTests 1 $ property $ do
  subKey {msg = Msg} None === Nothing

ext_subkey_batch : Property
ext_subkey_batch = withTests 1 $ property $ do
  subKey (Batch {msg = Msg} []) === Nothing

ext_subkey_every_includes_period : Property
ext_subkey_every_includes_period = withTests 1 $ property $ do
  (subKey (Every "t" 1000 tick') == subKey (Every "t" 500 tick')) === False

ext_subkey_port_includes_name : Property
ext_subkey_port_includes_name = withTests 1 $ property $ do
  (subKey (Port "p" "ws1" port') == subKey (Port "p" "ws2" port')) === False

--------------------------------------------------------------------------------
-- diffSubs.
--------------------------------------------------------------------------------

ext_diff_empty_empty : Property
ext_diff_empty_empty = withTests 1 $ property $ do
  let r = diffSubs {msg = Msg} [] []
  installCbs r === []
  teardownCbs r === []

ext_diff_all_new : Property
ext_diff_all_new = withTests 1 $ property $ do
  let r = diffSubs [] [OnKeyDown "k" key', Every "t" 1000 tick']
  installCbs r === ["k", "t"]
  teardownCbs r === []

ext_diff_all_gone : Property
ext_diff_all_gone = withTests 1 $ property $ do
  let r = diffSubs [OnKeyDown "k" key'] []
  installCbs r === []
  teardownCbs r === ["k"]

ext_diff_survivor_noop : Property
ext_diff_survivor_noop = withTests 1 $ property $ do
  let r = diffSubs [Every "t" 1000 tick'] [Every "t" 1000 tick']
  installCbs r === []
  teardownCbs r === []

ext_diff_period_change_reinstalls : Property
ext_diff_period_change_reinstalls = withTests 1 $ property $ do
  let r = diffSubs [Every "t" 1000 tick'] [Every "t" 500 tick']
  installCbs r === ["t"]
  teardownCbs r === ["t"]

ext_diff_portname_change_reinstalls : Property
ext_diff_portname_change_reinstalls = withTests 1 $ property $ do
  let r = diffSubs [Port "p" "ws1" port'] [Port "p" "ws2" port']
  installCbs r === ["p"]
  teardownCbs r === ["p"]

ext_diff_projection_change_is_noop : Property
ext_diff_projection_change_is_noop = withTests 1 $ property $ do
  -- Same cbId, same params, different projection => no browser churn.
  let r = diffSubs [OnKeyDown "k" key'] [OnKeyDown "k" (key' . (++ "!"))]
  installCbs r === []
  teardownCbs r === []

ext_diff_mixed : Property
ext_diff_mixed = withTests 1 $ property $ do
  let prev = [OnKeyDown "k" key', Every "t" 1000 tick', Port "p" "ws" port']
  let next = [OnKeyDown "k" key', Every "t" 500 tick', OnAnimationFrame "a" tick']
  let r = diffSubs prev next
  installCbs r === ["t", "a"]    -- t reinstalled (period), a brand new
  teardownCbs r === ["t", "p"]   -- t torn down (period), p vanished; k survives

--------------------------------------------------------------------------------
-- PBT: identical prev/next leaf sets are always a no-op.
--------------------------------------------------------------------------------

genCb : Gen String
genCb = string (linear 1 6) alphaNum

pbt_survivors_never_touched : Property
pbt_survivors_never_touched = property $ do
  cbs <- forAll (list (linear 0 8) genCb)
  let subs = map (\c => OnKeyDown c key') cbs
  let r = diffSubs subs subs
  installCbs r === []
  teardownCbs r === []

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "TEAWeb.SubDiff"
  [ ("ext_subkey_none", ext_subkey_none)
  , ("ext_subkey_batch", ext_subkey_batch)
  , ("ext_subkey_every_includes_period", ext_subkey_every_includes_period)
  , ("ext_subkey_port_includes_name", ext_subkey_port_includes_name)
  , ("ext_diff_empty_empty", ext_diff_empty_empty)
  , ("ext_diff_all_new", ext_diff_all_new)
  , ("ext_diff_all_gone", ext_diff_all_gone)
  , ("ext_diff_survivor_noop", ext_diff_survivor_noop)
  , ("ext_diff_period_change_reinstalls", ext_diff_period_change_reinstalls)
  , ("ext_diff_portname_change_reinstalls", ext_diff_portname_change_reinstalls)
  , ("ext_diff_projection_change_is_noop", ext_diff_projection_change_is_noop)
  , ("ext_diff_mixed", ext_diff_mixed)
  , ("pbt_survivors_never_touched", pbt_survivors_never_touched)
  ]
