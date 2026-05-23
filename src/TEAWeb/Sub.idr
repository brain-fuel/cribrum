||| TEAWeb.Sub — subscription descriptions (Phase T4, MVP slice).
|||
||| `Sub msg` describes what passive event streams the app wants to be
||| informed about (key presses, animation frames, websocket messages
||| via ports, etc). The runtime installs/un-installs listeners as the
||| Sub tree changes between renders.
|||
||| MVP variant set is intentionally narrow: `None` and `Batch` only.
||| The Counter+Focus demo uses `None`. Sub-leaf variants (`OnKeyDown`,
||| `OnAnimationFrame`, `Every`, `OnResize`, `Port`) arrive with the
||| TEAWeb T6 demo (docs nav island) which actually needs them.
module TEAWeb.Sub

%default total

||| Subscription tree. Pure data; interpreted by `TEAWeb.Runtime`.
public export
data Sub : Type -> Type where
  ||| No subscriptions.
  None  : Sub msg
  ||| A batch of subscriptions; all installed concurrently.
  Batch : List (Sub msg) -> Sub msg
