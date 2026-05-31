||| Pure-logic tests for the in-place reconcile decision core in
||| `Cribrum.Render.Dom`: `attrDomName`, `diffAttrs`, `hasDuplicateKeys`,
||| `chooseChildStrategy`.
|||
||| The reconcile itself drives the DOM via FFI and is chez-untestable
||| (browser demo). What these tests pin is which attribute mutations a
||| render needs and which child-diff strategy applies — pure functions
||| over HExpr / HAttr.
module Test.TEAWeb.AttrDiff

import Hedgehog
import Data.List
import Cribrum.Node
import Cribrum.Render.Dom
import TEAWeb.Html

%default total

--------------------------------------------------------------------------------
-- Fixtures.
--------------------------------------------------------------------------------

str : String -> String -> HAttr
str n v = MkHAttr n (Str v)

hdl : String -> String -> HAttr
hdl ev cb = MkHAttr ("on" ++ ev) (Handler ev cb)

keyedLi : String -> String -> HExpr
keyedLi k txt = tree (the (View ()) (li_ [key_ k] [text_ txt]))

unkeyedLi : String -> HExpr
unkeyedLi txt = tree (the (View ()) (li_ [] [text_ txt]))

--------------------------------------------------------------------------------
-- attrDomName.
--------------------------------------------------------------------------------

ext_domname_str : Property
ext_domname_str = withTests 1 $ property $
  attrDomName (str "class" "x") === "class"

ext_domname_handler : Property
ext_domname_handler = withTests 1 $ property $
  attrDomName (hdl "click" "cb1") === "data-on-click"

--------------------------------------------------------------------------------
-- diffAttrs.
--------------------------------------------------------------------------------

ext_diff_no_change : Property
ext_diff_no_change = withTests 1 $ property $
  diffAttrs [str "class" "a"] [str "class" "a"] === []

ext_diff_value_change : Property
ext_diff_value_change = withTests 1 $ property $
  diffAttrs [str "class" "a"] [str "class" "b"] === [SetStr "class" "b"]

ext_diff_add : Property
ext_diff_add = withTests 1 $ property $
  diffAttrs [] [str "id" "x"] === [SetStr "id" "x"]

ext_diff_remove : Property
ext_diff_remove = withTests 1 $ property $
  diffAttrs [str "id" "x"] [] === [RemoveAttr "id"]

ext_diff_handler_add : Property
ext_diff_handler_add = withTests 1 $ property $
  diffAttrs [] [hdl "click" "cb1"] === [AddHandler "click" "data-on-click" "cb1"]

ext_diff_handler_cbid_change : Property
ext_diff_handler_cbid_change = withTests 1 $ property $
  -- same event, new cbId => UpdateHandler only (live shim, no re-attach)
  diffAttrs [hdl "click" "cb1"] [hdl "click" "cb2"]
    === [UpdateHandler "data-on-click" "cb2"]

ext_diff_handler_unchanged : Property
ext_diff_handler_unchanged = withTests 1 $ property $
  diffAttrs [hdl "click" "cb1"] [hdl "click" "cb1"] === []

ext_diff_handler_remove : Property
ext_diff_handler_remove = withTests 1 $ property $
  diffAttrs [hdl "click" "cb1"] [] === [RemoveHandler "data-on-click"]

ext_diff_str_to_handler_flip : Property
ext_diff_str_to_handler_flip = withTests 1 $ property $
  -- A Str under data-on-click replaced by a Handler under the same DOM
  -- name: the handler is added (attaches the listener); no stale remove
  -- needed because the DOM name is still present.
  diffAttrs [MkHAttr "data-on-click" (Str "cb1")] [hdl "click" "cb2"]
    === [AddHandler "click" "data-on-click" "cb2"]

--------------------------------------------------------------------------------
-- hasDuplicateKeys.
--------------------------------------------------------------------------------

ext_dupkeys_none : Property
ext_dupkeys_none = withTests 1 $ property $
  hasDuplicateKeys [keyedLi "a" "A", keyedLi "b" "B"] === False

ext_dupkeys_yes : Property
ext_dupkeys_yes = withTests 1 $ property $
  hasDuplicateKeys [keyedLi "a" "A", keyedLi "a" "B"] === True

ext_dupkeys_ignores_unkeyed : Property
ext_dupkeys_ignores_unkeyed = withTests 1 $ property $
  hasDuplicateKeys [unkeyedLi "A", unkeyedLi "B"] === False

--------------------------------------------------------------------------------
-- chooseChildStrategy.
--------------------------------------------------------------------------------

ext_strategy_keyed : Property
ext_strategy_keyed = withTests 1 $ property $
  chooseChildStrategy [keyedLi "a" "A"] [keyedLi "a" "A", keyedLi "b" "B"]
    === Keyed

ext_strategy_unkeyed_forces_positional : Property
ext_strategy_unkeyed_forces_positional = withTests 1 $ property $
  chooseChildStrategy [keyedLi "a" "A", unkeyedLi "x"] [keyedLi "a" "A"]
    === Positional

ext_strategy_dup_forces_positional : Property
ext_strategy_dup_forces_positional = withTests 1 $ property $
  chooseChildStrategy [keyedLi "a" "A", keyedLi "a" "B"] [keyedLi "a" "A"]
    === Positional

ext_strategy_empty_is_keyed : Property
ext_strategy_empty_is_keyed = withTests 1 $ property $
  -- vacuously uniform on both sides
  chooseChildStrategy [] [] === Keyed

--------------------------------------------------------------------------------
-- PBT: an identical attr list always diffs to no edits.
--------------------------------------------------------------------------------

genName : Gen String
genName = string (linear 1 5) alpha

pbt_identity_no_edits : Property
pbt_identity_no_edits = property $ do
  names <- forAll (list (linear 0 6) genName)
  -- de-dup names so each maps to a distinct DOM attribute
  let attrs = map (\n => str n (n ++ "v")) (nub names)
  diffAttrs attrs attrs === []

--------------------------------------------------------------------------------
-- Group.
--------------------------------------------------------------------------------

export
group : Group
group = MkGroup "TEAWeb.AttrDiff"
  [ ("ext_domname_str", ext_domname_str)
  , ("ext_domname_handler", ext_domname_handler)
  , ("ext_diff_no_change", ext_diff_no_change)
  , ("ext_diff_value_change", ext_diff_value_change)
  , ("ext_diff_add", ext_diff_add)
  , ("ext_diff_remove", ext_diff_remove)
  , ("ext_diff_handler_add", ext_diff_handler_add)
  , ("ext_diff_handler_cbid_change", ext_diff_handler_cbid_change)
  , ("ext_diff_handler_unchanged", ext_diff_handler_unchanged)
  , ("ext_diff_handler_remove", ext_diff_handler_remove)
  , ("ext_diff_str_to_handler_flip", ext_diff_str_to_handler_flip)
  , ("ext_dupkeys_none", ext_dupkeys_none)
  , ("ext_dupkeys_yes", ext_dupkeys_yes)
  , ("ext_dupkeys_ignores_unkeyed", ext_dupkeys_ignores_unkeyed)
  , ("ext_strategy_keyed", ext_strategy_keyed)
  , ("ext_strategy_unkeyed_forces_positional", ext_strategy_unkeyed_forces_positional)
  , ("ext_strategy_dup_forces_positional", ext_strategy_dup_forces_positional)
  , ("ext_strategy_empty_is_keyed", ext_strategy_empty_is_keyed)
  , ("pbt_identity_no_edits", pbt_identity_no_edits)
  ]
