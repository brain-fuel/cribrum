||| EXT + PDDT for `TEAWeb.Sub`.
|||
||| Sub is pure data — tests interrogate its leaf-flattening behaviour
||| and the cbId projection. Runtime install lands with the T6 demo and
||| is exercised there (chez-untestable FFI surface).
module Test.TEAWeb.Sub

import Hedgehog
import TEAWeb.Sub

%default total

data Msg = MA | MFromString String | MFromDouble Double

Eq Msg where
  MA == MA = True
  MFromString a == MFromString b = a == b
  MFromDouble  a == MFromDouble  b = a == b
  _  == _  = False

Show Msg where
  show MA = "MA"
  show (MFromString s) = "MFromString " ++ show s
  show (MFromDouble d) = "MFromDouble " ++ show d

--------------------------------------------------------------------------------
-- EXTs — each leaf flattens to a singleton.
--------------------------------------------------------------------------------

export
ext_flatten_none : Property
ext_flatten_none = withTests 1 . property $ do
  length (flatten (the (Sub Msg) None)) === 0

export
ext_flatten_onKeyDown : Property
ext_flatten_onKeyDown = withTests 1 . property $ do
  length (flatten (the (Sub Msg) (OnKeyDown "k" MFromString))) === 1

export
ext_flatten_onAnimationFrame : Property
ext_flatten_onAnimationFrame = withTests 1 . property $ do
  length (flatten (the (Sub Msg) (OnAnimationFrame "f" MFromDouble))) === 1

export
ext_flatten_every : Property
ext_flatten_every = withTests 1 . property $ do
  length (flatten (the (Sub Msg) (Every "e" 1000 MFromDouble))) === 1

export
ext_flatten_port : Property
ext_flatten_port = withTests 1 . property $ do
  length (flatten (the (Sub Msg) (Port "p" "ws" MFromString))) === 1

export
ext_flatten_batch_collapses : Property
ext_flatten_batch_collapses = withTests 1 . property $ do
  let sub : Sub Msg
      sub = Batch
              [ OnKeyDown "k" MFromString
              , Batch [ Every "e" 16 MFromDouble
                      , OnAnimationFrame "f" MFromDouble
                      ]
              , None
              ]
  length (flatten sub) === 3

export
ext_flatten_drops_none : Property
ext_flatten_drops_none = withTests 1 . property $ do
  let sub : Sub Msg
      sub = Batch [None, None, OnKeyDown "k" MFromString, None]
  length (flatten sub) === 1

--------------------------------------------------------------------------------
-- cbId projection.
--------------------------------------------------------------------------------

export
ext_cbId_none_is_nothing : Property
ext_cbId_none_is_nothing = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) None) === Nothing

export
ext_cbId_batch_is_nothing : Property
ext_cbId_batch_is_nothing = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (Batch [])) === Nothing

export
ext_cbId_onKeyDown : Property
ext_cbId_onKeyDown = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (OnKeyDown "kd" MFromString)) === Just "kd"

export
ext_cbId_every : Property
ext_cbId_every = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (Every "tick" 500 MFromDouble)) === Just "tick"

export
ext_cbId_port : Property
ext_cbId_port = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (Port "p" "ws" MFromString)) === Just "p"

--------------------------------------------------------------------------------
-- PDDT — common Sub shapes flatten to expected leaf counts.
--------------------------------------------------------------------------------

cases : List (Sub Msg, Nat)
cases =
  [ (None,                                            0)
  , (OnKeyDown "k" MFromString,                       1)
  , (OnAnimationFrame "f" MFromDouble,                1)
  , (Every "e" 100 MFromDouble,                       1)
  , (Port "p" "ws" MFromString,                       1)
  , (Batch [],                                        0)
  , (Batch [None],                                    0)
  , (Batch [OnKeyDown "k" MFromString, Every "e" 1 MFromDouble], 2)
  , (Batch [Batch [Port "a" "x" MFromString, Port "b" "y" MFromString], OnKeyDown "k" MFromString], 3)
  ]

export
pddt_flatten_lengths : Property
pddt_flatten_lengths = withTests 1 . property $
  for_ cases $ \(s, expectedLen) =>
    length (flatten s) === expectedLen

--------------------------------------------------------------------------------
-- PBT — flatten of nested Batch chains never introduces None.
--------------------------------------------------------------------------------

countNone : List (Sub Msg) -> Nat
countNone [] = 0
countNone (None :: ss) = S (countNone ss)
countNone (_    :: ss) = countNone ss

genCbId : Gen String
genCbId = Gen.string (Range.linear 1 5) Gen.alphaNum

export
pbt_flatten_strips_none : Property
pbt_flatten_strips_none = property $ do
  ids <- forAll $ Gen.list (Range.linear 0 5) genCbId
  let subs : List (Sub Msg)
      subs = map (\cb => OnKeyDown cb MFromString) ids
      out  = flatten (Batch (None :: subs))
  countNone out === 0
  length out === length ids

export
group : Group
group = MkGroup "TEAWeb.Sub"
  [ ("ext_flatten_none",              ext_flatten_none)
  , ("ext_flatten_onKeyDown",         ext_flatten_onKeyDown)
  , ("ext_flatten_onAnimationFrame",  ext_flatten_onAnimationFrame)
  , ("ext_flatten_every",             ext_flatten_every)
  , ("ext_flatten_port",              ext_flatten_port)
  , ("ext_flatten_batch_collapses",   ext_flatten_batch_collapses)
  , ("ext_flatten_drops_none",        ext_flatten_drops_none)
  , ("ext_cbId_none_is_nothing",      ext_cbId_none_is_nothing)
  , ("ext_cbId_batch_is_nothing",     ext_cbId_batch_is_nothing)
  , ("ext_cbId_onKeyDown",            ext_cbId_onKeyDown)
  , ("ext_cbId_every",                ext_cbId_every)
  , ("ext_cbId_port",                 ext_cbId_port)
  , ("pddt_flatten_lengths",          pddt_flatten_lengths)
  , ("pbt_flatten_strips_none",       pbt_flatten_strips_none)
  ]
