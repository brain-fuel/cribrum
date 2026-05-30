||| EXT + PDDT for the keyed-reconcile pure helpers in
||| `Cribrum.Render.Dom` (`hexprKey`, `reusableKeys`).
|||
||| The reconcile itself drives the DOM via FFI and is chez-untestable
||| (it lands in the T6 demo / a browser harness). Its decision core —
||| which keyed children may reuse their live node across a render — is
||| pure and is what these tests pin: reuse iff a key appears on both
||| sides with a byte-identical subtree.
module Test.TEAWeb.Keyed

import Hedgehog
import Data.List
import Cribrum.Node
import Cribrum.Render.Dom
import TEAWeb.Html

%default total

-- A keyed <li> with the given key + text content. Built through the
-- view layer so `data-key` lands exactly as the runtime would emit it.
-- `()` pins the otherwise-free `msg` type variable (these views carry
-- no handlers).
keyedLi : String -> String -> HExpr
keyedLi k txt = tree (the (View ()) (li_ [key_ k] [text_ txt]))

unkeyedLi : String -> HExpr
unkeyedLi txt = tree (the (View ()) (li_ [] [text_ txt]))

--------------------------------------------------------------------------------
-- hexprKey: reads data-key off an element, Nothing otherwise.
--------------------------------------------------------------------------------

export
ext_hexprKey_present : Property
ext_hexprKey_present = withTests 1 . property $ do
  hexprKey (keyedLi "a" "Apple") === Just "a"

export
ext_hexprKey_absent : Property
ext_hexprKey_absent = withTests 1 . property $ do
  hexprKey (unkeyedLi "Apple") === Nothing

export
ext_hexprKey_text_node : Property
ext_hexprKey_text_node = withTests 1 . property $ do
  hexprKey (Text "x") === Nothing

--------------------------------------------------------------------------------
-- reusableKeys: a key is reusable iff it appears on both sides with an
-- identical subtree.
--------------------------------------------------------------------------------

export
ext_reuse_identical_reorder : Property
ext_reuse_identical_reorder = withTests 1 . property $ do
  -- Same three keyed items, re-ordered: all three reuse their nodes.
  let prev = [keyedLi "a" "A", keyedLi "b" "B", keyedLi "c" "C"]
  let next = [keyedLi "c" "C", keyedLi "a" "A", keyedLi "b" "B"]
  sort (reusableKeys prev next) === ["a", "b", "c"]

export
ext_reuse_changed_content_excluded : Property
ext_reuse_changed_content_excluded = withTests 1 . property $ do
  -- "b" keeps its key but its content changed: NOT reusable (rebuilt).
  let prev = [keyedLi "a" "A", keyedLi "b" "B"]
  let next = [keyedLi "a" "A", keyedLi "b" "B!"]
  reusableKeys prev next === ["a"]

export
ext_reuse_new_key_excluded : Property
ext_reuse_new_key_excluded = withTests 1 . property $ do
  -- "z" is brand new: not reusable. "a" survives identical.
  let prev = [keyedLi "a" "A"]
  let next = [keyedLi "a" "A", keyedLi "z" "Z"]
  reusableKeys prev next === ["a"]

export
ext_reuse_removed_key_excluded : Property
ext_reuse_removed_key_excluded = withTests 1 . property $ do
  -- "b" vanished from next: not in the reuse set (its node is removed).
  let prev = [keyedLi "a" "A", keyedLi "b" "B"]
  let next = [keyedLi "a" "A"]
  reusableKeys prev next === ["a"]

export
ext_reuse_unkeyed_never : Property
ext_reuse_unkeyed_never = withTests 1 . property $ do
  -- Unkeyed siblings never reuse, even if identical.
  let prev = [unkeyedLi "A", keyedLi "k" "K"]
  let next = [unkeyedLi "A", keyedLi "k" "K"]
  reusableKeys prev next === ["k"]

export
ext_reuse_empty : Property
ext_reuse_empty = withTests 1 . property $ do
  reusableKeys [] [] === []

export
group : Group
group = MkGroup "TEAWeb.Keyed"
  [ ("ext_hexprKey_present",              ext_hexprKey_present)
  , ("ext_hexprKey_absent",               ext_hexprKey_absent)
  , ("ext_hexprKey_text_node",            ext_hexprKey_text_node)
  , ("ext_reuse_identical_reorder",       ext_reuse_identical_reorder)
  , ("ext_reuse_changed_content_excluded", ext_reuse_changed_content_excluded)
  , ("ext_reuse_new_key_excluded",        ext_reuse_new_key_excluded)
  , ("ext_reuse_removed_key_excluded",    ext_reuse_removed_key_excluded)
  , ("ext_reuse_unkeyed_never",           ext_reuse_unkeyed_never)
  , ("ext_reuse_empty",                   ext_reuse_empty)
  ]
