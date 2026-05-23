||| EXT + PDDT + PBT for `TEAWeb.Cmd`.
|||
||| Cmd is pure data — tests interrogate its structure. Effect
||| interpretation (`runCmd`) is FFI-only and not exercised here; it
||| lands in the chez-untestable Phase T6 demo.
module Test.TEAWeb.Cmd

import Hedgehog
import TEAWeb.Cmd

%default total

data Msg = MA | MB

Eq Msg where
  MA == MA = True
  MB == MB = True
  _  == _  = False

Show Msg where
  show MA = "MA"
  show MB = "MB"

-- Structural-equality witness for Cmd Msg, for tests only. Real-world
-- code never compares Cmds.
cmdEq : Cmd Msg -> Cmd Msg -> Bool
cmdEq None         None         = True
cmdEq (Focus a)    (Focus b)    = a == b
cmdEq (Blur  a)    (Blur  b)    = a == b
cmdEq (Batch as)   (Batch bs)   = case (as, bs) of
  ([], [])           => True
  (x :: xs, y :: ys) => assert_total (cmdEq x y) && assert_total (cmdEq (Batch xs) (Batch ys))
  _                  => False
cmdEq _            _            = False

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

export
ext_flatten_none : Property
ext_flatten_none = withTests 1 . property $ do
  length (flatten (the (Cmd Msg) None)) === 0

export
ext_flatten_leaf : Property
ext_flatten_leaf = withTests 1 . property $ do
  length (flatten (the (Cmd Msg) (Focus "x"))) === 1

export
ext_flatten_batch_collapses : Property
ext_flatten_batch_collapses = withTests 1 . property $ do
  let cmd : Cmd Msg
      cmd = Batch
              [ Focus "a"
              , Batch [Blur "b", Focus "c"]
              , None
              ]
  length (flatten cmd) === 3

export
ext_flatten_drops_none : Property
ext_flatten_drops_none = withTests 1 . property $ do
  let cmd : Cmd Msg
      cmd = Batch [None, None, Focus "x", None]
  length (flatten cmd) === 1

--------------------------------------------------------------------------------
-- PDDT — common Cmd shapes flatten to expected lengths.
--------------------------------------------------------------------------------

cases : List (Cmd Msg, Nat)
cases =
  [ (None,                                          0)
  , (Focus "a",                                     1)
  , (Blur  "b",                                     1)
  , (Batch [],                                      0)
  , (Batch [None],                                  0)
  , (Batch [Focus "a", Blur "b"],                   2)
  , (Batch [Batch [Focus "a", Focus "b"], Focus "c"], 3)
  ]

export
pddt_flatten_lengths : Property
pddt_flatten_lengths = withTests 1 . property $
  for_ cases $ \(c, expectedLen) =>
    length (flatten c) === expectedLen

--------------------------------------------------------------------------------
-- PBT — flatten of nested Batch chains never introduces None.
--------------------------------------------------------------------------------

countNone : List (Cmd Msg) -> Nat
countNone [] = 0
countNone (None :: cs) = S (countNone cs)
countNone (_    :: cs) = countNone cs

genElementId : Gen String
genElementId = Gen.string (Range.linear 1 5) Gen.alphaNum

export
pbt_flatten_strips_none : Property
pbt_flatten_strips_none = property $ do
  ids <- forAll $ Gen.list (Range.linear 0 5) genElementId
  let cmds : List (Cmd Msg)
      cmds = map Focus ids
      out  = flatten (Batch (None :: cmds))
  countNone out === 0
  length out === length ids

export
group : Group
group = MkGroup "TEAWeb.Cmd"
  [ ("ext_flatten_none",            ext_flatten_none)
  , ("ext_flatten_leaf",            ext_flatten_leaf)
  , ("ext_flatten_batch_collapses", ext_flatten_batch_collapses)
  , ("ext_flatten_drops_none",      ext_flatten_drops_none)
  , ("pddt_flatten_lengths",        pddt_flatten_lengths)
  , ("pbt_flatten_strips_none",     pbt_flatten_strips_none)
  ]
