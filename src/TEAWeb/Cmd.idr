||| TEAWeb.Cmd — effect descriptions (Phase T4, MVP slice).
|||
||| `Cmd msg` describes a side effect the runtime should perform on
||| behalf of `update`. `update` returns it as plain data; the runtime
||| (`TEAWeb.Runtime`) interprets it at the FFI boundary so `update`
||| stays pure and total.
|||
||| MVP variants: `None`, `Batch`, `Focus`, `Blur`. The plan-locked full
||| inventory adds `Http`, `Random`, `After`, `SendPort`; they slot in
||| here without disturbing the existing variants — every existing total
||| match over `Cmd` keeps compiling because the additions are new
||| constructors, not changes to old ones.
module TEAWeb.Cmd

%default total

||| Element id used to target a specific DOM element from a Cmd.
public export
ElementId : Type
ElementId = String

||| HTTP request method. A small closed set keeps the FFI shim simple;
||| anything outside it falls back to `GET` at the boundary.
public export
data HttpMethod = GET | POST | PUT | DELETE | PATCH

||| Wire name for an `HttpMethod`, as passed to `fetch`.
public export
methodName : HttpMethod -> String
methodName GET    = "GET"
methodName POST   = "POST"
methodName PUT    = "PUT"
methodName DELETE = "DELETE"
methodName PATCH  = "PATCH"

||| One request header (name, value). Rendered into the `fetch`
||| `headers` object at the FFI boundary.
public export
HttpHeader : Type
HttpHeader = (String, String)

||| The outcome of an `Http` request, handed to the app's response
||| projection. `HttpOk` carries the response body text + status code;
||| `HttpErr` carries a diagnostic string (network failure, non-2xx
||| status that the shim chose to surface as an error, JSON parse, ...).
||| The app decides how to fold each into a `msg`.
public export
data HttpResult : Type where
  HttpOk  : (status : Int) -> (body : String) -> HttpResult
  HttpErr : (reason : String) -> HttpResult

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
  ||| Perform an HTTP request via `fetch`. The runtime issues the request
  ||| asynchronously; when it settles it projects the `HttpResult` to a
  ||| `msg` (via the supplied function) and feeds it back through the
  ||| dispatch loop exactly like any other message. `body` is sent
  ||| verbatim; the empty string means no request body.
  Http  : (method : HttpMethod)
       -> (url : String)
       -> (headers : List HttpHeader)
       -> (body : String)
       -> (onResult : HttpResult -> msg)
       -> Cmd msg
  ||| Generate a pseudo-random integer uniformly in `[lo, hi]` (inclusive)
  ||| and project it to a `msg`. Backed by `Math.random` at the boundary.
  Random : (lo : Integer) -> (hi : Integer) -> (onValue : Integer -> msg) -> Cmd msg
  ||| Emit `msg` after `delayMs` milliseconds (`setTimeout`). One-shot;
  ||| for a repeating tick use the `Every` subscription instead.
  After  : (delayMs : Integer) -> (onElapsed : msg) -> Cmd msg
  ||| Send `payload` (already-serialised JSON / text) out on the named
  ||| outbound port. The host page reads it via a registered port
  ||| callback. The Idris↔JS boundary in one direction; see `TEAWeb.Ports`.
  SendPort : (portName : String) -> (payload : String) -> Cmd msg

||| Flatten a `Batch` tree to a `List (Cmd msg)` containing no further
||| `Batch` constructors. Helpful for the interpreter, also useful in
||| tests that assert a Cmd contains a specific leaf.
public export
flatten : Cmd msg -> List (Cmd msg)
flatten None         = []
flatten (Batch cmds) = assert_total (concatMap flatten cmds)
flatten leaf         = [leaf]
