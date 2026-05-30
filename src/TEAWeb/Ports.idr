||| TEAWeb.Ports — the Idris↔JS port boundary (Phase T5).
|||
||| Elm-style ports give an app a typed, declarative seam to the host
||| page's JavaScript without poking holes in `update`/`view`. Two
||| directions:
|||
|||   * *Outbound* — `send name payload` is a `Cmd msg`. When the runtime
|||     interprets it, every JS callback the host registered for the
|||     named port (via `window.__cribrumOutPorts[name].push(fn)`) is
|||     invoked with `payload`. Fire-and-forget; no msg comes back from a
|||     send.
|||
|||   * *Inbound* — `subscribe name toMsg` is a `Sub msg`. The runtime
|||     installs a receiver under `window.__cribrumPorts[name]`; when the
|||     host calls it with a string, the runtime stashes the payload and
|||     dispatches `toMsg payload` into the update loop.
|||
||| Payloads are plain strings on the wire — callers serialise to / parse
||| from JSON themselves (a typed-JSON layer is a later, additive
||| refinement). This keeps the FFI surface a single `String` in each
||| direction and leaves encoding policy to the app.
|||
||| The module is intentionally thin: it is a naming + documentation
||| layer over the `Cmd.SendPort` and `Sub.Port` constructors, which
||| carry the actual runtime wiring. Factoring it out means app code
||| reads `Ports.send "log" msg` / `Ports.subscribe "ws" GotWs` rather
||| than reaching for raw constructors, and gives one place to grow the
||| typed-JSON story.
module TEAWeb.Ports

import TEAWeb.Cmd
import TEAWeb.Sub

%default total

||| A port's name. Outbound and inbound ports share a namespace on the
||| JS side (`window.__cribrumOutPorts` / `window.__cribrumPorts`), but
||| a given name is conventionally used in exactly one direction.
public export
PortName : Type
PortName = String

--------------------------------------------------------------------------------
-- Outbound: app → host JS.
--------------------------------------------------------------------------------

||| Send `payload` out on the named port. Produces a `Cmd msg` the
||| runtime interprets by invoking each registered host-side subscriber
||| for `name`. The result type is polymorphic in `msg` because a send
||| never produces a message of its own.
|||
||| Host page registers a receiver like:
|||
|||     window.__cribrumOutPorts = window.__cribrumOutPorts || {};
|||     (window.__cribrumOutPorts.log ||= []).push(s => console.log(s));
public export
send : PortName -> (payload : String) -> Cmd msg
send = SendPort

--------------------------------------------------------------------------------
-- Inbound: host JS → app.
--------------------------------------------------------------------------------

||| Subscribe to inbound messages on the named port. Produces a
||| `Sub msg`; each delivery runs `toMsg payload` and feeds the result
||| into `update`. `callbackId` must be unique among the app's
||| subscriptions (it keys the runtime's handler table); by convention
||| it is `"port:" ++ name`.
|||
||| Host page pushes a message like:
|||
|||     window.__cribrumPorts.ws("{\"type\":\"tick\"}");
public export
subscribe : (callbackId : String) -> PortName -> (toMsg : String -> msg) -> Sub msg
subscribe cb name toMsg = Port cb name toMsg

||| Convenience: `subscribe` with the conventional `"port:" ++ name`
||| callback id, so callers that don't need to manage ids by hand can
||| just name the port once.
public export
subscribeNamed : PortName -> (toMsg : String -> msg) -> Sub msg
subscribeNamed name toMsg = Port ("port:" ++ name) name toMsg
