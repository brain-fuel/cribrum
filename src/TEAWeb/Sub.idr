||| TEAWeb.Sub — subscription descriptions (Phase T4).
|||
||| `Sub msg` describes what passive event streams the app wants to be
||| informed about (key presses, animation frames, periodic ticks,
||| websocket messages via ports, ...). The runtime installs/un-installs
||| listeners as the Sub tree changes between renders.
|||
||| Each leaf variant carries an app-supplied `callbackId` plus the
||| projection that turns the event payload into a `msg`. Mirrors the
||| `TEAWeb.Event` attribute shape so the dispatcher can reuse the same
||| `(String, Event -> IO msg)` handler-table machinery.
|||
||| Per plan.dj §Phase T4, the leaf set is closed:
|||
|||     OnKeyDown          — `keydown` on the document; payload is `event.key`.
|||     OnAnimationFrame   — one fire per `requestAnimationFrame`; payload is
|||                          the high-res `DOMHighResTimeStamp` (ms since
|||                          navigation start).
|||     Every              — `setInterval`-style periodic tick at the given
|||                          period in milliseconds; payload is `Date.now()`.
|||     Port               — message arrived on a named JSON port; payload is
|||                          the raw JSON string (the Phase T5 `Ports`
|||                          module will sharpen this to typed JSON).
|||
||| Runtime install of leaf variants lands with the T6 docs-nav demo; the
||| current Counter+Focus MVP only uses `None`. The data type is fixed
||| here so callers can build the values today, and adding install hooks
||| later is additive (no change to this surface).
module TEAWeb.Sub

%default total

||| Subscription tree. Pure data; interpreted by `TEAWeb.Runtime`.
public export
data Sub : Type -> Type where
  ||| No subscriptions.
  None             : Sub msg
  ||| A batch of subscriptions; all installed concurrently.
  Batch            : List (Sub msg) -> Sub msg
  ||| Document-level `keydown` listener. Payload is `event.key`.
  OnKeyDown        : (callbackId : String) -> (String -> msg) -> Sub msg
  ||| One fire per `requestAnimationFrame`. Payload is the rAF
  ||| `DOMHighResTimeStamp` argument (ms since navigation start).
  OnAnimationFrame : (callbackId : String) -> (Double -> msg) -> Sub msg
  ||| Periodic tick fired every `period` milliseconds. Payload is
  ||| `Date.now()` at the moment the tick runs.
  Every            : (callbackId : String) -> (period : Integer)
                  -> (Double -> msg) -> Sub msg
  ||| Inbound message on the named port. Payload is the raw JSON
  ||| string; Phase T5 `Ports` will lift this to a typed value.
  Port             : (callbackId : String) -> (portName : String)
                  -> (String -> msg) -> Sub msg

||| Flatten a `Batch` tree to a list of leaf subscriptions. Strips
||| `None` and concatenates nested `Batch`es; returned list contains
||| only leaf variants. Mirrors `Cmd.flatten` — used by the runtime to
||| compute the install set for a given render, and by tests that
||| assert a Sub tree's leaf shape.
public export
flatten : Sub msg -> List (Sub msg)
flatten None         = []
flatten (Batch subs) = assert_total (concatMap flatten subs)
flatten leaf         = [leaf]

||| The callback id carried by a leaf variant, or `Nothing` for the
||| compound `None` / `Batch` cases. Convenience for the runtime's
||| install / teardown bookkeeping.
public export
subCallbackId : Sub msg -> Maybe String
subCallbackId None                       = Nothing
subCallbackId (Batch _)                  = Nothing
subCallbackId (OnKeyDown cb _)           = Just cb
subCallbackId (OnAnimationFrame cb _)    = Just cb
subCallbackId (Every cb _ _)             = Just cb
subCallbackId (Port  cb _ _)             = Just cb
