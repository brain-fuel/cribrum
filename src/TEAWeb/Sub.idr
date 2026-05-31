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

import Data.List
import Data.Maybe

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
  ||| Document-level `keyup` listener. Payload is `event.key`.
  OnKeyUp          : (callbackId : String) -> (String -> msg) -> Sub msg
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
subCallbackId (OnKeyUp   cb _)           = Just cb
subCallbackId (OnAnimationFrame cb _)    = Just cb
subCallbackId (Every cb _ _)             = Just cb
subCallbackId (Port  cb _ _)             = Just cb

--------------------------------------------------------------------------------
-- Subscription diff (Phase T4 runtime support).
--
-- `Sub msg` holds projection functions, so it has no `Eq`. To diff the
-- previous installed set against a freshly-evaluated one we key each leaf
-- by its *structural identity* — the callback id plus the non-function
-- fields whose change requires a browser-side reinstall (an `Every`'s
-- period, a `Port`'s name). The projection is deliberately excluded: a
-- projection change needs no reinstall, because the dispatcher always
-- invokes the *current* closure rebuilt from the latest leaf set.
--------------------------------------------------------------------------------

||| Structural identity of a leaf subscription: enough to decide whether
||| two leaves across renders are "the same browser resource".
public export
data SubKey
  = KKeyDown String          -- cbId
  | KKeyUp   String          -- cbId
  | KAnim    String          -- cbId
  | KEvery   String Integer  -- cbId + period (period change => reinstall)
  | KPort    String String   -- cbId + portName (portName change => reinstall)

public export
Eq SubKey where
  KKeyDown a == KKeyDown b = a == b
  KKeyUp   a == KKeyUp   b = a == b
  KAnim    a == KAnim    b = a == b
  KEvery a p == KEvery b q = a == b && p == q
  KPort  a n == KPort  b m = a == b && n == m
  _          == _          = False

public export
Show SubKey where
  show (KKeyDown a) = "KKeyDown " ++ a
  show (KKeyUp   a) = "KKeyUp " ++ a
  show (KAnim    a) = "KAnim " ++ a
  show (KEvery a p) = "KEvery " ++ a ++ " " ++ show p
  show (KPort  a n) = "KPort " ++ a ++ " " ++ n

||| The structural key of a leaf; `Nothing` for the `None` / `Batch`
||| structural nodes (which `flatten` strips before any diff).
export
subKey : Sub msg -> Maybe SubKey
subKey None                    = Nothing
subKey (Batch _)               = Nothing
subKey (OnKeyDown cb _)        = Just (KKeyDown cb)
subKey (OnKeyUp   cb _)        = Just (KKeyUp cb)
subKey (OnAnimationFrame cb _) = Just (KAnim cb)
subKey (Every cb p _)          = Just (KEvery cb p)
subKey (Port  cb n _)          = Just (KPort cb n)

||| Diff two *flattened* leaf lists (as from `flatten`). Returns the
||| leaves to install (present in `next` with a key absent from `prev`)
||| and the callback ids to tear down (present in `prev` with a key
||| absent from `next`).
|||
||| A leaf whose `SubKey` is unchanged is a no-op: its browser listener
||| is already live and is left untouched. A same-cbId leaf with changed
||| params (e.g. `Every "t" 1000` -> `Every "t" 500`) has a *different*
||| key on each side, so it appears in BOTH lists — the runtime tears the
||| old resource down first, then installs the new one, leaving exactly
||| one live resource at the new params.
export
diffSubs : List (Sub msg) -> List (Sub msg)
        -> (List (Sub msg), List String)
diffSubs prev next =
  let prevKeys = mapMaybe subKey prev
      nextKeys = mapMaybe subKey next
      toInstall  = filter (\s => case subKey s of
                                   Nothing => False
                                   Just k  => not (elem k prevKeys)) next
      toTeardown = mapMaybe (\s => case subKey s of
                                     Nothing => Nothing
                                     Just k  => if elem k nextKeys
                                                  then Nothing
                                                  else subCallbackId s) prev
   in (toInstall, toTeardown)
