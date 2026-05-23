||| TEAWeb.Cmd — effect descriptions (Phase T4, MVP slice).
|||
||| `Cmd msg` describes a side effect the runtime should perform on
||| behalf of `update`. `update` returns it as plain data; the runtime
||| (`TEAWeb.Runtime`) interprets it at the FFI boundary so `update`
||| stays pure and total.
|||
||| MVP variants: `None`, `Batch`, `Focus`, `Blur`. The plan-locked full
||| inventory adds `Http`, `Random`, `After`; they slot in here without
||| disturbing the existing variants.
module TEAWeb.Cmd

%default total

||| Element id used to target a specific DOM element from a Cmd.
public export
ElementId : Type
ElementId = String

||| A side effect the runtime will perform. Pure data; interpreted by
||| `TEAWeb.Runtime`. Producing one is non-observable from `update`'s
||| perspective.
public export
data Cmd : Type -> Type where
  ||| No effect.
  None  : Cmd msg
  ||| A batch of independent effects performed in declaration order.
  Batch : List (Cmd msg) -> Cmd msg
  ||| Focus the element with this id (`element.focus()` at the FFI boundary).
  Focus : ElementId -> Cmd msg
  ||| Blur the element with this id (`element.blur()`).
  Blur  : ElementId -> Cmd msg

||| Flatten a `Batch` tree to a `List (Cmd msg)` containing no further
||| `Batch` constructors. Helpful for the interpreter, also useful in
||| tests that assert a Cmd contains a specific leaf.
public export
flatten : Cmd msg -> List (Cmd msg)
flatten None         = []
flatten (Batch cmds) = assert_total (concatMap flatten cmds)
flatten leaf         = [leaf]
