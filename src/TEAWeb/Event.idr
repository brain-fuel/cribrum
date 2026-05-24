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

-- `eventTargetValue` is re-exported from `TEAWeb.Html` (where Event
-- itself lives — see the note in TEAWeb.Html on the chez cross-module
-- FFI restriction).

--------------------------------------------------------------------------------
-- Core event helpers. Each takes an app-supplied callback id + a
-- function `Event -> msg`; specialised wrappers below cover the common
-- shapes.
--------------------------------------------------------------------------------

||| Most general form: pick an event name + callback id + IO-producing
||| function returning `msg`. Most code should use the specialised
||| helpers (`onClick`, etc) rather than this directly.
public export
on : (event : String) -> (callbackId : String) -> (Event -> IO msg) -> Attr msg
on = On

--------------------------------------------------------------------------------
-- click / submit / focus / blur / mouse — pure-msg form (event ignored).
--------------------------------------------------------------------------------

||| `onClick id msg` registers a click handler that emits `msg`. The id
||| must be unique per render — TEAWeb relies on it to address the
||| handler when the click fires.
public export
onClick : (callbackId : String) -> msg -> Attr msg
onClick cb m = On "click" cb (\_ => pure m)

||| `onSubmit id msg` registers a submit handler that emits `msg`.
public export
onSubmit : (callbackId : String) -> msg -> Attr msg
onSubmit cb m = On "submit" cb (\_ => pure m)

||| `onDoubleClick id msg` — emit `msg` on `dblclick`.
public export
onDoubleClick : (callbackId : String) -> msg -> Attr msg
onDoubleClick cb m = On "dblclick" cb (\_ => pure m)

||| `onFocus id msg` — emit `msg` on `focus`.
public export
onFocus : (callbackId : String) -> msg -> Attr msg
onFocus cb m = On "focus" cb (\_ => pure m)

||| `onBlur id msg` — emit `msg` on `blur`.
public export
onBlur : (callbackId : String) -> msg -> Attr msg
onBlur cb m = On "blur" cb (\_ => pure m)

||| `onMouseEnter id msg`.
public export
onMouseEnter : (callbackId : String) -> msg -> Attr msg
onMouseEnter cb m = On "mouseenter" cb (\_ => pure m)

||| `onMouseLeave id msg`.
public export
onMouseLeave : (callbackId : String) -> msg -> Attr msg
onMouseLeave cb m = On "mouseleave" cb (\_ => pure m)

--------------------------------------------------------------------------------
-- input / change — value-extracting form. The closure reads
-- `event.target.value` via `eventTargetValue` (FFI) and then applies
-- the supplied `String -> msg`.
--------------------------------------------------------------------------------

||| `onInput id f` — emit `f value` when the input element's value
||| changes (`input` event). `value` is the string content of
||| `event.target.value`.
public export
onInput : (callbackId : String) -> (String -> msg) -> Attr msg
onInput cb f = On "input" cb (\e => do v <- eventTargetValue e; pure (f v))

||| `onChange id f` — emit `f value` on `change`.
public export
onChange : (callbackId : String) -> (String -> msg) -> Attr msg
onChange cb f = On "change" cb (\e => do v <- eventTargetValue e; pure (f v))
