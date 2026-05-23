||| TEAWeb.Event — typed event-handler attribute helpers (Phase T2).
|||
||| Each helper produces an `Attr msg` of the `On` shape, pairing the
||| HTML-side wire format (`data-on-<event>="<callbackId>"`) with the
||| msg-producing closure (`Event -> msg`). The runtime's dispatch
||| closure resolves a fired event by looking the callback id up in the
||| current view's `HandlerTable msg` and pushing the resulting `msg`
||| onto the update queue.
|||
||| Callback ids are app-supplied as strings for the MVP spike. Phase 4
||| of the diff strategy (when keyed-children reconciliation lands)
||| migrates these to deterministic `hash(path, event)` ids assigned by
||| a view-walk pre-pass; the public API of these helpers stays the same
||| (`app supplies a unique tag` becomes `framework supplies a unique
||| tag`).
module TEAWeb.Event

import TEAWeb.Html

%default total

--------------------------------------------------------------------------------
-- Core event helpers. Each takes an app-supplied callback id + a
-- function `Event -> msg`; specialised wrappers below cover the common
-- shapes.
--------------------------------------------------------------------------------

||| Most general form: pick an event name + callback id + msg-producing
||| function. Most code should use the specialised helpers
||| (`onClick`, etc) rather than this directly.
public export
on : (event : String) -> (callbackId : String) -> (Event -> msg) -> Attr msg
on = On

--------------------------------------------------------------------------------
-- click / submit (single-msg form — event ignored).
--------------------------------------------------------------------------------

||| `onClick id msg` registers a click handler that emits `msg`. The id
||| must be unique per render — TEAWeb relies on it to address the
||| handler when the click fires.
public export
onClick : (callbackId : String) -> msg -> Attr msg
onClick cb m = On "click" cb (\_ => m)

||| `onSubmit id msg` registers a submit handler that emits `msg`.
public export
onSubmit : (callbackId : String) -> msg -> Attr msg
onSubmit cb m = On "submit" cb (\_ => m)

||| `onDoubleClick id msg` — emit `msg` on `dblclick`.
public export
onDoubleClick : (callbackId : String) -> msg -> Attr msg
onDoubleClick cb m = On "dblclick" cb (\_ => m)

||| `onFocus id msg` — emit `msg` on `focus`.
public export
onFocus : (callbackId : String) -> msg -> Attr msg
onFocus cb m = On "focus" cb (\_ => m)

||| `onBlur id msg` — emit `msg` on `blur`.
public export
onBlur : (callbackId : String) -> msg -> Attr msg
onBlur cb m = On "blur" cb (\_ => m)

||| `onMouseEnter id msg`.
public export
onMouseEnter : (callbackId : String) -> msg -> Attr msg
onMouseEnter cb m = On "mouseenter" cb (\_ => m)

||| `onMouseLeave id msg`.
public export
onMouseLeave : (callbackId : String) -> msg -> Attr msg
onMouseLeave cb m = On "mouseleave" cb (\_ => m)

--------------------------------------------------------------------------------
-- input / change (Event-payload form — value extracted from the event
-- via TEAWeb.Runtime).
--
-- The closure stores `(Event -> msg)`; for inputs we want
-- `(String -> msg)` after extracting `event.target.value`. The
-- extraction happens at the FFI boundary in `TEAWeb.Runtime`, which
-- supplies the closure here with the already-extracted string wrapped
-- back into an opaque Event payload via a runtime convention. Until
-- Runtime lands, these helpers are declared by signature only and the
-- closure unused-warning is suppressed by explicitly ignoring the Event.
--
-- This is the *only* place in TEAWeb where the convention deviates
-- from a literal closure on Event — and it disappears once Runtime
-- ships, with no callsite churn.
--------------------------------------------------------------------------------

||| `onInput id f` — emit `f value` when the input element's value
||| changes. `value` is the string content of the input field.
|||
||| NOTE — spike: the closure currently always receives an unused Event
||| placeholder. TEAWeb.Runtime will swap to extracted-string dispatch.
public export
onInput : (callbackId : String) -> (String -> msg) -> Attr msg
onInput cb f = On "input" cb (\_ => f "")

||| `onChange id f` — emit `f value` on `change`.
public export
onChange : (callbackId : String) -> (String -> msg) -> Attr msg
onChange cb f = On "change" cb (\_ => f "")
