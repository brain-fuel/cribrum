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

-- Raw passthrough: parse an HTML string into a DocumentFragment via a
-- detached <template>, returning the fragment. appendChild on a fragment
-- splices its children in place, so a multi-node raw block lands correctly.
%foreign "browser:lambda:(html)=>{ const t=document.createElement('template'); t.innerHTML=html; return t.content; }"
prim__createRawFragment : String -> PrimIO DomNode

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

%foreign "scheme:(lambda (_) \"\")"
         "browser:lambda:(_)=>String(window.__cribrumKey || \"\")"
prim__currentEventKey : String -> PrimIO String

||| String content of the most recent event's `event.key`, as stashed
||| by `TEAWeb.Runtime.installDispatch`. Used by keyboard-event helpers
||| (`onKeyDown`, `onKeyUp`). Returns the empty string when no key has
||| been recorded yet (pre-mount, non-keyboard events).
export
currentEventKey : IO String
currentEventKey = fromPrim (prim__currentEventKey "")

%foreign "scheme:(lambda (_) 0.0)"
         "browser:lambda:(_)=>Number(window.__cribrumTimestamp || 0)"
prim__currentEventTimestamp : String -> PrimIO Double

||| Numeric timestamp (DOMHighResTimeStamp or `Date.now()` value) most
||| recently stashed by the Sub installer for an animation-frame or
||| `Every` tick. Returns `0.0` outside such a dispatch.
export
currentEventTimestamp : IO Double
currentEventTimestamp = fromPrim (prim__currentEventTimestamp "")

%foreign "scheme:(lambda (_) \"\")"
         "browser:lambda:(_)=>String(window.__cribrumPortMsg || \"\")"
prim__currentEventPortMsg : String -> PrimIO String

||| String payload of the most recent inbound port message, as stashed
||| by the port installer. Returns the empty string outside a port
||| dispatch.
export
currentEventPortMsg : IO String
currentEventPortMsg = fromPrim (prim__currentEventPortMsg "")

--------------------------------------------------------------------------------
-- Focus preservation across blow-and-rebuild reconcile.
--
-- Day-1 reconcile destroys + recreates every DOM node. DOM-resident
-- state (focus, selection range, scroll position) is lost. For
-- controlled inputs this means every keystroke yanks the cursor out
-- of the field. We side-step it by snapshotting `document.activeEl-
-- ement.id` + selection range into a window-scoped slot before the
-- blow, and re-applying them after the rebuild. Day-2 keyed-children
-- diff makes this unnecessary; until then the workaround is a one-
-- line bracket around the reconcile body.
--------------------------------------------------------------------------------

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(_)=>{ var a=document.activeElement; if(a&&a.id){ window.__cribrumFocusId=a.id; window.__cribrumSelStart=(typeof a.selectionStart==='number')?a.selectionStart:0; window.__cribrumSelEnd=(typeof a.selectionEnd==='number')?a.selectionEnd:0; } else { window.__cribrumFocusId=''; } return 0; }"
prim__captureFocus : String -> PrimIO Int

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(_)=>{ var id=window.__cribrumFocusId; if(id){ var el=document.getElementById(id); if(el){ if(typeof el.focus==='function') el.focus(); if(typeof el.setSelectionRange==='function'){ try { el.setSelectionRange(window.__cribrumSelStart, window.__cribrumSelEnd); } catch(e){} } } } return 0; }"
prim__restoreFocus : String -> PrimIO Int

||| Snapshot `document.activeElement.id` + selection range into a
||| window slot so `restoreFocus` can put the cursor back after the
||| host's children get re-rendered. No-op when no element is focused
||| or the focused element has no `id`.
export
captureFocus : IO ()
captureFocus = ignore (fromPrim (prim__captureFocus ""))

||| Re-focus the element whose id was stashed by `captureFocus` and
||| restore its selection range. No-op when the snapshot is empty or
||| the element has gone away (the new render didn't recreate it
||| under the same id).
export
restoreFocus : IO ()
restoreFocus = ignore (fromPrim (prim__restoreFocus ""))

--------------------------------------------------------------------------------
-- Scroll-position preservation across blow-and-rebuild reconcile.
--
-- Same motivation as focus preservation: the Day-1 reconcile discards
-- every DOM node, so any scrollable container's `scrollTop` /
-- `scrollLeft` resets to 0 even if the new tree recreates the
-- container under the same id. Snapshot every id'd element's scroll
-- offsets into a window-scoped map before the blow, restore after.
--
-- Targets EVERY id'd element rather than only the obvious scroll
-- containers because `overflow: auto` may be applied via CSS the
-- renderer can't see; storing offsets unconditionally costs almost
-- nothing and a non-scrollable element ignores the restored value.
-- Day-2 keyed-children diff makes this unnecessary.
--------------------------------------------------------------------------------

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(_)=>{ var m={}; var all=document.querySelectorAll('[id]'); for(var i=0;i<all.length;i++){ var el=all[i]; m[el.id]={t:el.scrollTop||0,l:el.scrollLeft||0}; } window.__cribrumScrolls=m; return 0; }"
prim__captureScrolls : String -> PrimIO Int

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(_)=>{ var m=window.__cribrumScrolls||{}; for(var id in m){ if(!Object.prototype.hasOwnProperty.call(m,id)) continue; var el=document.getElementById(id); if(el){ try { el.scrollTop=m[id].t; el.scrollLeft=m[id].l; } catch(e){} } } return 0; }"
prim__restoreScrolls : String -> PrimIO Int

||| Snapshot every id'd element's `scrollTop`/`scrollLeft` into a
||| window-scoped map keyed by id. Pairs with `restoreScrolls` around
||| a reconcile.
export
captureScrolls : IO ()
captureScrolls = ignore (fromPrim (prim__captureScrolls ""))

||| Re-apply scroll offsets snapshotted by `captureScrolls`. Elements
||| missing from the new render are silently skipped.
export
restoreScrolls : IO ()
restoreScrolls = ignore (fromPrim (prim__restoreScrolls ""))

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
createRawFragment : String -> IO DomNode
createRawFragment s = fromPrim (prim__createRawFragment s)

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
renderDom (Raw s)     = createRawFragment s
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
  captureFocus
  captureScrolls
  clearChildren host
  fresh <- renderDom next
  appendChild host fresh
  restoreScrolls
  restoreFocus

||| Convenience for an initial mount with no previous tree.
export
mountInto : (host : DomNode) -> (tree : HExpr) -> IO ()
mountInto host tree = do
  clearChildren host
  node <- renderDom tree
  appendChild host node
