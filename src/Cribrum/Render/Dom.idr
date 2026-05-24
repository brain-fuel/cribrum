||| DOM renderer — Phase 5 (P5.1 + P5.2 + P5.3 spike).
|||
||| Tiny FFI surface to the browser DOM, per plan.dj §Phase 5. Every
||| primitive is a single-line `%foreign` binding; impurity is threaded
||| through `IO`/`PrimIO`. The bindings target the Idris 2 JavaScript
||| backend ("browser:lambda:" prefix) and are intentionally not callable
||| from the chez/native backend — Cribrum's chez test suite type-checks
||| this module but never executes its FFI leaves. End-to-end execution
||| arrives with the TEAWeb MVP demo (Phase T runtime + `examples/teaweb/
||| counter`), which is the first chez-untestable artifact in the project.
|||
||| Handler-attr wiring contract: an attribute like
|||   data-on-click="cb-42"
||| is *not* installed automatically by `renderDom`. Instead the renderer
||| registers each handler attr via `prim__addEventListener` so that, at
||| event time, the browser invokes a globally-registered dispatcher with
||| the callback id. TEAWeb.Runtime supplies the dispatcher; Render.Dom
||| only knows the wire format. This keeps the IR, the renderer, and the
||| TEA loop fully decoupled.
|||
||| Diff strategy (P5.3): Day-1 blow-and-rebuild — `reconcile` clears the
||| host element's children and re-renders. The whole diff strategy is
||| confined to that one function, so swapping to keyed-children + shallow
||| attribute diff (or a real VDOM, if a measurable need ever forces it)
||| is a local change.
module Cribrum.Render.Dom

import Data.List
import Data.String
import Cribrum.Node

%default total

--------------------------------------------------------------------------------
-- Opaque DOM handle.
--------------------------------------------------------------------------------

||| A live reference to a DOM node. Under the JS backend this is a JS
||| object reference (`Element`, `Text`, `Comment`, etc.); under chez/
||| native the type exists only for type-checking — there is no path
||| from chez code to a value of this type because the constructors are
||| exclusively manufactured by `%foreign` primitives that the chez
||| backend cannot execute.
public export
data DomNode : Type where [external]

--------------------------------------------------------------------------------
-- FFI primitives. One JS expression per declaration. Total at Idris.
--------------------------------------------------------------------------------

%foreign "browser:lambda:(tag)=>document.createElement(tag)"
prim__createElement : String -> PrimIO DomNode

%foreign "browser:lambda:(text)=>document.createTextNode(text)"
prim__createTextNode : String -> PrimIO DomNode

%foreign "browser:lambda:(text)=>document.createComment(text)"
prim__createComment : String -> PrimIO DomNode

%foreign "browser:lambda:(node,name,value)=>node.setAttribute(name,value)"
prim__setAttribute : DomNode -> String -> String -> PrimIO ()

%foreign "browser:lambda:(node,name)=>node.removeAttribute(name)"
prim__removeAttribute : DomNode -> String -> PrimIO ()

%foreign "browser:lambda:(node,evt,cbId)=>node.addEventListener(evt,(e)=>{ if(typeof window.__cribrumDispatch==='function'){ window.__cribrumDispatch(cbId,e) } })"
prim__addEventListener : DomNode -> String -> String -> PrimIO ()

%foreign "browser:lambda:(parent,child)=>parent.appendChild(child)"
prim__appendChild : DomNode -> DomNode -> PrimIO ()

%foreign "browser:lambda:(parent,newChild,oldChild)=>parent.replaceChild(newChild,oldChild)"
prim__replaceChild : DomNode -> DomNode -> DomNode -> PrimIO ()

%foreign "browser:lambda:(id)=>document.getElementById(id)"
prim__getElementById : String -> PrimIO DomNode

%foreign "browser:lambda:(parent)=>{ while(parent.firstChild){ parent.removeChild(parent.firstChild) } }"
prim__clearChildren : DomNode -> PrimIO ()

||| Read `window.__cribrumValue`, which the TEAWeb runtime populates
||| just before invoking the Idris callback for an input/change event.
||| Lives in `Cribrum.Render.Dom` because the chez backend rejects
||| `browser:lambda:` primitives that originate in TEAWeb modules even
||| though identical primitives in Cribrum modules link cleanly — the
||| primer-chain difference is a chez quirk we work around here.
||| `TEAWeb.Html.eventTargetValue` re-exports this.
%foreign "scheme:(lambda (_) \"\")"
         "browser:lambda:(_)=>String(window.__cribrumValue || \"\")"
prim__currentEventValue : String -> PrimIO String

||| String content of the most recent event's `target.value`, as
||| stashed by `TEAWeb.Runtime.installDispatch`. Returns the empty
||| string when no value has been recorded yet (pre-mount, non-input
||| events).
export
currentEventValue : IO String
currentEventValue = fromPrim (prim__currentEventValue "")

--------------------------------------------------------------------------------
-- IO wrappers (the layer above the FFI; this is what renderDom uses).
--------------------------------------------------------------------------------

export
createElement : String -> IO DomNode
createElement t = fromPrim (prim__createElement t)

export
createTextNode : String -> IO DomNode
createTextNode s = fromPrim (prim__createTextNode s)

export
createComment : String -> IO DomNode
createComment s = fromPrim (prim__createComment s)

export
setAttribute : DomNode -> String -> String -> IO ()
setAttribute n k v = fromPrim (prim__setAttribute n k v)

export
removeAttribute : DomNode -> String -> IO ()
removeAttribute n k = fromPrim (prim__removeAttribute n k)

||| Register a handler attribute on a DOM node. `event` is the DOM event
||| name (`"click"`, `"input"`, ...); `callbackId` is the opaque id that
||| Cribrum's renderer also writes to `data-on-<event>` for visibility.
||| At event-time, the JS shim looks up `window.__cribrumDispatch` and
||| hands it `(callbackId, event)` — TEAWeb's runtime installs that
||| dispatcher. If no dispatcher is registered the event is dropped
||| silently (so a pre-mount Render.Dom usage never errors).
export
addEventListener : DomNode -> (event : String) -> (callbackId : String) -> IO ()
addEventListener n ev cb = fromPrim (prim__addEventListener n ev cb)

export
appendChild : DomNode -> DomNode -> IO ()
appendChild p c = fromPrim (prim__appendChild p c)

export
replaceChild : (parent : DomNode) -> (newChild : DomNode) -> (oldChild : DomNode) -> IO ()
replaceChild p n o = fromPrim (prim__replaceChild p n o)

export
getElementById : String -> IO DomNode
getElementById i = fromPrim (prim__getElementById i)

export
clearChildren : DomNode -> IO ()
clearChildren n = fromPrim (prim__clearChildren n)

--------------------------------------------------------------------------------
-- Render: HExpr -> DOM.
--------------------------------------------------------------------------------

||| Per `Render.Html`, the wire format for a handler attribute is
||| `data-on-<event>="<callbackId>"`. We split that here so the DOM render
||| can both write the attribute (for inspectability + parity with the
||| string renderer) AND register the event listener via FFI.
applyAttr : DomNode -> HAttr -> IO ()
applyAttr node (MkHAttr name (Str value)) =
  setAttribute node name value
applyAttr node (MkHAttr _ (Handler event callbackId)) = do
  setAttribute node ("data-on-" ++ event) callbackId
  addEventListener node event callbackId

||| Total `HExpr -> IO DomNode`. Mirrors `Render.Html` structurally so
||| the two renderers agree on shape — `assert_total` is used only on
||| the structural recursion into `children`, identical to how
||| `Render.Html` handles it.
export
renderDom : HExpr -> IO DomNode
renderDom (Text s)    = createTextNode s
renderDom (Comment s) = createComment s
renderDom (Element tag attrs children) = do
  node <- createElement tag
  -- Attach attrs (incl. handler attrs).
  traverse_ (applyAttr node) attrs
  -- Append rendered children in source order.
  childNodes <- assert_total (traverse renderDom children)
  traverse_ (appendChild node) childNodes
  pure node

--------------------------------------------------------------------------------
-- Reconcile: swap one tree for another under a host node.
--------------------------------------------------------------------------------

||| Day-1 implementation: blow away the host's children and rebuild from
||| scratch. The `_previous` HExpr is accepted now for signature stability
||| — Day-2 (keyed-children + shallow attr diff) will use it.
|||
||| This is the *only* function in the renderer that has to change when
||| diff strategy evolves. Mount, render, and FFI all stay put.
export
reconcile : (host : DomNode) -> (previous : HExpr) -> (next : HExpr) -> IO ()
reconcile host _ next = do
  clearChildren host
  fresh <- renderDom next
  appendChild host fresh

||| Convenience for an initial mount with no previous tree.
export
mountInto : (host : DomNode) -> (tree : HExpr) -> IO ()
mountInto host tree = do
  clearChildren host
  node <- renderDom tree
  appendChild host node
