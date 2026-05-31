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
import Data.Maybe
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

-- The listener reads the *current* callback id from the node's
-- `data-on-<event>` attribute on every dispatch rather than capturing
-- `cbId` by value. This makes the listener stable across in-place
-- reconciles: an attribute-only diff that rewrites `data-on-<event>`
-- redirects the handler with no need to remove/re-add the listener. A
-- handler removed from the view clears the attribute, and the `c` guard
-- makes the now-orphaned listener inert. `cbId` is still passed (and
-- written to the attribute by `applyAttr`) so a freshly-rendered node
-- dispatches correctly before any diff runs.
%foreign "browser:lambda:(node,evt,cbId)=>node.addEventListener(evt,(e)=>{ var c=node.getAttribute&&node.getAttribute('data-on-'+evt); if(c&&typeof window.__cribrumDispatch==='function'){ window.__cribrumDispatch(c,e) } })"
prim__addEventListener : DomNode -> String -> String -> PrimIO ()

%foreign "browser:lambda:(parent,child)=>parent.appendChild(child)"
prim__appendChild : DomNode -> DomNode -> PrimIO ()

%foreign "browser:lambda:(parent,newChild,oldChild)=>parent.replaceChild(newChild,oldChild)"
prim__replaceChild : DomNode -> DomNode -> DomNode -> PrimIO ()

%foreign "browser:lambda:(id)=>document.getElementById(id)"
prim__getElementById : String -> PrimIO DomNode

-- Keyed-reconcile building blocks. Inert chez stubs (return the parent /
-- a dummy) so the module still type-checks at the chez backend; only the
-- JS backend runs them.

%foreign "scheme:(lambda (p,c,r) p)"
         "browser:lambda:(parent,newChild,refChild)=>parent.insertBefore(newChild,refChild)"
prim__insertBefore : DomNode -> DomNode -> DomNode -> PrimIO DomNode

%foreign "scheme:(lambda (p,c) p)"
         "browser:lambda:(parent,child)=>parent.removeChild(child)"
prim__removeChild : DomNode -> DomNode -> PrimIO DomNode

%foreign "scheme:(lambda (n) \"\")"
         "browser:lambda:(node)=>{ var v=node.getAttribute && node.getAttribute('data-key'); return v===null||v===undefined?'':String(v); }"
prim__dataKey : DomNode -> PrimIO String

%foreign "scheme:(lambda (p) 0)"
         "browser:lambda:(parent)=>parent.childElementCount||0"
prim__childCount : DomNode -> PrimIO Int

%foreign "scheme:(lambda (p,i) p)"
         "browser:lambda:(parent,i)=>parent.children[i]"
prim__childAt : DomNode -> Int -> PrimIO DomNode

-- In-place-diff building blocks. `childNode*` walk ALL child nodes
-- (text + comment + element), unlike `childCount`/`childAt` which are
-- element-only — the positional differ must line up against text nodes
-- too, e.g. `<p>hi <b>x</b></p>`. `setNodeValue` updates a Text/Comment
-- node's content without replacing the node (preserves identity).

%foreign "scheme:(lambda (n,s) 0)"
         "browser:lambda:(node,text)=>{ node.nodeValue=text; return 0; }"
prim__setNodeValue : DomNode -> String -> PrimIO Int

%foreign "scheme:(lambda (p) 0)"
         "browser:lambda:(parent)=>parent.childNodes.length"
prim__childNodeCount : DomNode -> PrimIO Int

%foreign "scheme:(lambda (p,i) p)"
         "browser:lambda:(parent,i)=>parent.childNodes[i]"
prim__childNodeAt : DomNode -> Int -> PrimIO DomNode

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
-- Async Cmd effect primitives (Http / Random / After / SendPort).
--
-- The async effects (Http, After) settle off the call stack. Each takes
-- a `callbackId`: when the effect resolves it stashes its result into a
-- window slot (`__cribrumHttp*` for Http) and invokes
-- `window.__cribrumDispatch(callbackId, {})`. The TEAWeb runtime
-- registers a one-shot projection closure under that id; the closure
-- reads the slot back out and folds the result into a `msg`. Random is
-- synchronous (no dispatch round-trip) and returns its value directly.
-- SendPort is fire-and-forget. Chez stubs are inert.
--------------------------------------------------------------------------------

%foreign "scheme:(lambda (_,_,_,_,_) 0)"
         "browser:lambda:(cbId,method,url,headers,body)=>{ try { var hs = JSON.parse(headers||'{}'); } catch(e){ var hs = {}; } var opts = { method: method || 'GET', headers: hs }; if (body && body.length > 0) { opts.body = body; } fetch(url, opts).then((resp)=>{ return resp.text().then((t)=>{ window.__cribrumHttpStatus = resp.status; window.__cribrumHttpBody = String(t); window.__cribrumHttpErr = ''; if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }); }).catch((err)=>{ window.__cribrumHttpStatus = 0; window.__cribrumHttpBody = ''; window.__cribrumHttpErr = String((err && err.message) || err || 'network error'); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }); return 0; }"
prim__httpFetch : String -> String -> String -> String -> String -> PrimIO Int

%foreign "scheme:(lambda (_,_,_) 0)"
         "browser:lambda:(cbId,delayMs,_)=>{ setTimeout(()=>{ if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }, Number(delayMs)); return 0; }"
prim__setTimeoutDispatch : String -> Integer -> String -> PrimIO Int

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } return 0; }"
prim__dispatchNow : String -> PrimIO Int

%foreign "scheme:(lambda (lo,_) lo)"
         "browser:lambda:(lo,hi)=>{ var a = Number(lo); var b = Number(hi); if (b < a) { var t = a; a = b; b = t; } return a + Math.floor(Math.random() * (b - a + 1)); }"
prim__randomInt : Integer -> Integer -> PrimIO Integer

%foreign "scheme:(lambda (_,_) 0)"
         "browser:lambda:(portName,payload)=>{ window.__cribrumOutPorts = window.__cribrumOutPorts || {}; var subs = window.__cribrumOutPorts[portName] || []; for (var i = 0; i < subs.length; i++) { try { subs[i](payload); } catch(e){} } return 0; }"
prim__sendPort : String -> String -> PrimIO Int

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(_)=>Number(window.__cribrumHttpStatus || 0)"
prim__currentHttpStatus : String -> PrimIO Int

%foreign "scheme:(lambda (_) \"\")"
         "browser:lambda:(_)=>String(window.__cribrumHttpBody || \"\")"
prim__currentHttpBody : String -> PrimIO String

%foreign "scheme:(lambda (_) \"\")"
         "browser:lambda:(_)=>String(window.__cribrumHttpErr || \"\")"
prim__currentHttpErr : String -> PrimIO String

||| Issue an HTTP request via `fetch`. `headers` is a JSON object string
||| (`{"Content-Type":"application/json"}`); `body` empty means no body.
||| On settle, stashes status/body/err into window slots and calls
||| `window.__cribrumDispatch(callbackId, {})`.
export
httpFetch :  (callbackId : String) -> (method : String) -> (url : String)
          -> (headersJson : String) -> (body : String) -> IO ()
httpFetch cb m u h b = ignore (fromPrim (prim__httpFetch cb m u h b))

||| Fire `window.__cribrumDispatch(callbackId, {})` after `delayMs`
||| milliseconds (one-shot `setTimeout`).
export
setTimeoutDispatch : (callbackId : String) -> (delayMs : Integer) -> IO ()
setTimeoutDispatch cb d = ignore (fromPrim (prim__setTimeoutDispatch cb d ""))

||| Synchronously invoke `window.__cribrumDispatch(callbackId, {})`.
||| Used for effects whose payload is already known at call time
||| (e.g. a just-generated random value) so they can re-enter the
||| update loop through the same path as any other delivery.
export
dispatchNow : (callbackId : String) -> IO ()
dispatchNow cb = ignore (fromPrim (prim__dispatchNow cb))

||| A pseudo-random integer uniform in `[lo, hi]` inclusive. Synchronous.
export
randomInt : (lo : Integer) -> (hi : Integer) -> IO Integer
randomInt lo hi = fromPrim (prim__randomInt lo hi)

||| Send `payload` out on the named outbound port — invokes every
||| registered subscriber callback for that port. Fire-and-forget.
export
sendPort : (portName : String) -> (payload : String) -> IO ()
sendPort n p = ignore (fromPrim (prim__sendPort n p))

||| HTTP status code stashed by the most recent `httpFetch` settle.
export
currentHttpStatus : IO Int
currentHttpStatus = fromPrim (prim__currentHttpStatus "")

||| HTTP response body stashed by the most recent `httpFetch` settle.
export
currentHttpBody : IO String
currentHttpBody = fromPrim (prim__currentHttpBody "")

||| HTTP error string stashed by the most recent `httpFetch` settle
||| (empty on success).
export
currentHttpErr : IO String
currentHttpErr = fromPrim (prim__currentHttpErr "")

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

export
insertBefore : (parent : DomNode) -> (newChild : DomNode) -> (refChild : DomNode) -> IO DomNode
insertBefore p n r = fromPrim (prim__insertBefore p n r)

export
removeChild : (parent : DomNode) -> (child : DomNode) -> IO DomNode
removeChild p c = fromPrim (prim__removeChild p c)

||| Value of a node's `data-key` attribute, or "" if absent.
export
dataKey : DomNode -> IO String
dataKey n = fromPrim (prim__dataKey n)

||| Number of *element* children of a node (text/comment nodes excluded).
export
childCount : DomNode -> IO Int
childCount n = fromPrim (prim__childCount n)

||| The element child at index `i` (0-based; into `.children`).
export
childAt : DomNode -> Int -> IO DomNode
childAt n i = fromPrim (prim__childAt n i)

||| Set a Text/Comment node's content in place (no node replacement).
export
setNodeValue : DomNode -> String -> IO ()
setNodeValue n s = ignore (fromPrim (prim__setNodeValue n s))

||| Number of child *nodes* (text + comment + element) of a node.
export
childNodeCount : DomNode -> IO Int
childNodeCount n = fromPrim (prim__childNodeCount n)

||| The child *node* at index `i` (0-based; into `.childNodes`, so it
||| includes text and comment nodes — unlike `childAt`).
export
childNodeAt : DomNode -> Int -> IO DomNode
childNodeAt n i = fromPrim (prim__childNodeAt n i)

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

||| Convenience for an initial mount with no previous tree.
export
mountInto : (host : DomNode) -> (tree : HExpr) -> IO ()
mountInto host tree = do
  clearChildren host
  node <- renderDom tree
  appendChild host node

--------------------------------------------------------------------------------
-- Keyed-children reconcile (opt-in; additive to the default path).
--
-- The Day-1 `reconcile` above blows the whole subtree away on any
-- change, which destroys DOM identity: re-ordering a keyed list (drag,
-- sort, filter) rebuilds every row, losing focus / scroll / animation
-- state and re-running every listener attach. `reconcileKeyedChildren`
-- diffs ONE container's direct element children by their `data-key`
-- attribute, reusing the live DOM node for any key whose HExpr is
-- byte-identical between renders (so its already-attached listeners are
-- correct and need no re-attach — this is why we never call
-- `removeEventListener`: a reused node is unchanged, a changed node is
-- rendered fresh). Matched-but-moved nodes are repositioned with
-- `insertBefore`; new keys are rendered; vanished keys are removed.
--
-- Scope (deliberately bounded to land green): reuse is keyed +
-- whole-subtree-equality. A keyed node whose content changed is
-- re-rendered wholesale rather than diffed in place; that keeps the
-- listener story trivial. In-place attribute/child diffing of a changed
-- keyed node is the precise remaining work, noted in the runtime.
--------------------------------------------------------------------------------

||| The `data-key` of an HExpr element, or `Nothing` for unkeyed /
||| non-element nodes. Keyed reconcile only reuses element nodes that
||| carry a key on both sides.
export
hexprKey : HExpr -> Maybe String
hexprKey (Element _ attrs _) = go attrs
  where
    go : List HAttr -> Maybe String
    go []                          = Nothing
    go (MkHAttr "data-key" (Str v) :: _) = Just v
    go (_ :: rest)                 = go rest
hexprKey _ = Nothing

||| Look a key up in a `(key, node)` association list.
lookupKeyed : String -> List (String, DomNode) -> Maybe DomNode
lookupKeyed _ [] = Nothing
lookupKeyed k ((j, node) :: rest) =
  if k == j then Just node else lookupKeyed k rest

||| Snapshot a container's keyed element children over `[i, n)` into a
||| `(key, node)` list. Unkeyed (`data-key == ""`) children are dropped.
keyedSnapshot : DomNode -> Int -> Int -> IO (List (String, DomNode))
keyedSnapshot c i n =
  if i >= n
    then pure []
    else assert_total $ do
      child <- childAt c i
      k     <- dataKey child
      rest  <- keyedSnapshot c (i + 1) n
      pure (if k == "" then rest else (k, child) :: rest)

||| Keys carried (with byte-identical HExpr) by BOTH `previous` and
||| `next`. Only these are reuse candidates: a key whose subtree changed
||| is rebuilt, sidestepping any listener re-attach.
export
reusableKeys : List HExpr -> List HExpr -> List String
reusableKeys prevs nexts =
  let pk = mapMaybe (\h => map (\k => (k, h)) (hexprKey h)) prevs
   in mapMaybe
        (\h => case hexprKey h of
                 Nothing => Nothing
                 Just k  => case lookup k pk of
                              Just oldH => if oldH == h then Just k else Nothing
                              Nothing   => Nothing)
        nexts

||| Remove a snapshotted keyed node iff its key is not being reused.
removeStaleKeyed : (c : DomNode) -> (reusable : List String) -> (String, DomNode) -> IO ()
removeStaleKeyed c reusable (k, node) =
  if elem k reusable then pure () else ignore (removeChild c node)

--------------------------------------------------------------------------------
-- In-place diff (D2): pure edit computation + child strategy choice.
--------------------------------------------------------------------------------

||| The DOM attribute name an `HAttr` is written under by `applyAttr`: a
||| plain `Str` attr uses its own name; a `Handler` is written under the
||| synthetic `data-on-<event>` name (NOT the HAttr's `name` field, which
||| `applyAttr` ignores for handlers). The diff must match attrs by this
||| name to line up with how they were rendered.
export
attrDomName : HAttr -> String
attrDomName (MkHAttr name (Str _))         = name
attrDomName (MkHAttr _    (Handler ev _))  = "data-on-" ++ ev

||| A single attribute mutation produced by `diffAttrs`. `AddHandler`
||| (first appearance) attaches the listener; `UpdateHandler` only
||| rewrites the `data-on-<event>` attribute — the live-reading listener
||| shim picks up the new callback id with no re-attach. Removals clear
||| the attribute (a removed handler's orphaned listener reads a null
||| attribute and is inert).
public export
data AttrEdit
  = SetStr        String String        -- name, value
  | AddHandler    String String String -- event, data-on-name, cbId
  | UpdateHandler String String        -- data-on-name, cbId
  | RemoveAttr    String               -- DOM name
  | RemoveHandler String               -- data-on-name

public export
Eq AttrEdit where
  SetStr a b        == SetStr c d        = a == c && b == d
  AddHandler a b c  == AddHandler d e f  = a == d && b == e && c == f
  UpdateHandler a b == UpdateHandler c d = a == c && b == d
  RemoveAttr a      == RemoveAttr b      = a == b
  RemoveHandler a   == RemoveHandler b   = a == b
  _                 == _                 = False

public export
Show AttrEdit where
  show (SetStr a b)        = "SetStr " ++ show a ++ " " ++ show b
  show (AddHandler a b c)  = "AddHandler " ++ show a ++ " " ++ show b ++ " " ++ show c
  show (UpdateHandler a b) = "UpdateHandler " ++ show a ++ " " ++ show b
  show (RemoveAttr a)      = "RemoveAttr " ++ show a
  show (RemoveHandler a)   = "RemoveHandler " ++ show a

||| Find an attr in a list by its DOM name.
lookupAttrByDom : String -> List HAttr -> Maybe HAttr
lookupAttrByDom _    []        = Nothing
lookupAttrByDom dom (a :: as)  =
  if attrDomName a == dom then Just a else lookupAttrByDom dom as

||| The edit needed to bring a single `next` attr into being, given what
||| (if anything) was present under the same DOM name before.
editForNext : (prev : List HAttr) -> HAttr -> Maybe AttrEdit
editForNext prev a@(MkHAttr name (Str v)) =
  case lookupAttrByDom (attrDomName a) prev of
    Just old => if old == a then Nothing else Just (SetStr name v)
    Nothing  => Just (SetStr name v)
editForNext prev a@(MkHAttr _ (Handler ev cb)) =
  let dom = attrDomName a in
  case lookupAttrByDom dom prev of
    Just (MkHAttr _ (Handler ev' cb')) =>
      if ev == ev' && cb == cb' then Nothing else Just (UpdateHandler dom cb)
    Just _  => Just (AddHandler ev dom cb)   -- was a Str under this name: attach listener
    Nothing => Just (AddHandler ev dom cb)

||| The removal needed for a `prev` attr whose DOM name is gone from `next`.
removalForGone : (next : List HAttr) -> HAttr -> Maybe AttrEdit
removalForGone next a =
  let dom = attrDomName a in
  case lookupAttrByDom dom next of
    Just _  => Nothing
    Nothing => case a of
                 MkHAttr _ (Str _)       => Just (RemoveAttr dom)
                 MkHAttr _ (Handler _ _) => Just (RemoveHandler dom)

||| Minimal edit list turning the `previous` attr set into `next`. Pure;
||| matched by DOM name (so handlers line up under `data-on-<event>`).
export
diffAttrs : (previous : List HAttr) -> (next : List HAttr) -> List AttrEdit
diffAttrs previous next =
  mapMaybe (editForNext previous) next
    ++ mapMaybe (removalForGone next) previous

||| Whether a child list carries a duplicate `data-key` — if so the keyed
||| path can't trust first-match lookup and we fall back to positional.
export
hasDuplicateKeys : List HExpr -> Bool
hasDuplicateKeys hs = go (mapMaybe hexprKey hs)
  where
    go : List String -> Bool
    go []        = False
    go (k :: ks) = elem k ks || go ks

||| How to diff one container's children.
public export
data ChildStrategy = Keyed | Positional

public export
Eq ChildStrategy where
  Keyed      == Keyed      = True
  Positional == Positional = True
  _          == _          = False

public export
Show ChildStrategy where
  show Keyed      = "Keyed"
  show Positional = "Positional"

||| Choose the keyed strategy only when BOTH child lists are uniformly
||| element-keyed (every child is an element carrying a distinct
||| `data-key`). Any unkeyed/text/comment child, or a duplicate key, on
||| either side forces the positional path — the minimal-correct boundary
||| for mixed lists.
export
chooseChildStrategy : List HExpr -> List HExpr -> ChildStrategy
chooseChildStrategy prev next =
  if uniform prev && uniform next then Keyed else Positional
  where
    uniform : List HExpr -> Bool
    uniform hs =
      all (\h => isJust (hexprKey h)) hs && not (hasDuplicateKeys hs)

--------------------------------------------------------------------------------
-- In-place diff (D1 + D2): recursive node/children reconcile. Mutually
-- recursive, so grouped; `assert_total` guards the structural recursion
-- the way `renderDom` does.
--------------------------------------------------------------------------------

||| Apply one attribute edit to a live node.
applyAttrEdit : DomNode -> AttrEdit -> IO ()
applyAttrEdit node (SetStr name v)        = setAttribute node name v
applyAttrEdit node (AddHandler ev dom cb) = do
  setAttribute node dom cb
  addEventListener node ev cb
applyAttrEdit node (UpdateHandler dom cb) = setAttribute node dom cb
applyAttrEdit node (RemoveAttr dom)       = removeAttribute node dom
applyAttrEdit node (RemoveHandler dom)    = removeAttribute node dom

mutual
  ||| Diff one live node (`current`, representing `previous`) toward
  ||| `next`. Same-tag elements diff attrs + children in place; text /
  ||| comment content updates in place; anything else (tag change,
  ||| text<->element, Raw change) is replaced wholesale.
  export
  diffNode : (parent : DomNode) -> (current : DomNode)
          -> (previous : HExpr) -> (next : HExpr) -> IO ()
  diffNode parent current previous next =
    case (previous, next) of
      (Text a, Text b) =>
        when (a /= b) (setNodeValue current b)
      (Comment a, Comment b) =>
        when (a /= b) (setNodeValue current b)
      (Element t pa pcs, Element u na ncs) =>
        if t == u
          then do
            traverse_ (applyAttrEdit current) (diffAttrs pa na)
            assert_total (diffChildren current pcs ncs)
          else replaceWith parent current next
      _ =>
        if previous == next then pure () else replaceWith parent current next

  ||| Replace the live `current` node under `parent` with a fresh render
  ||| of `next`.
  replaceWith : (parent : DomNode) -> (current : DomNode) -> (next : HExpr) -> IO ()
  replaceWith parent current next = do
    fresh <- renderDom next
    ignore (replaceChild parent fresh current)

  ||| Diff a container's children, choosing the keyed or positional
  ||| strategy.
  export
  diffChildren : (parent : DomNode)
              -> (previous : List HExpr) -> (next : List HExpr) -> IO ()
  diffChildren parent previous next =
    case chooseChildStrategy previous next of
      Keyed      => reconcileKeyedChildren parent previous next
      Positional => diffChildrenPositional parent previous next 0

  ||| Positional child diff over `.childNodes` (element + text + comment),
  ||| lockstep by index. Shared prefix is diffed in place; a longer
  ||| `next` appends the tail; a longer `previous` removes its tail
  ||| (highest index first so earlier indices stay valid).
  diffChildrenPositional : (parent : DomNode)
                        -> (previous : List HExpr) -> (next : List HExpr)
                        -> (i : Int) -> IO ()
  diffChildrenPositional parent (p :: ps) (n :: ns) i = do
    live <- childNodeAt parent i
    assert_total (diffNode parent live p n)
    assert_total (diffChildrenPositional parent ps ns (i + 1))
  diffChildrenPositional parent [] (n :: ns) i = do
    fresh <- renderDom n
    appendChild parent fresh
    assert_total (diffChildrenPositional parent [] ns (i + 1))
  diffChildrenPositional parent prev [] i =
    -- Remove the leftover live tail [i, i+len prev). Remove from the end
    -- so earlier indices don't shift under us.
    removeTailFrom parent i (cast (length prev))
  diffChildrenPositional _ [] [] _ = pure ()

  ||| Remove `count` child nodes starting at `start`, highest index first.
  removeTailFrom : (parent : DomNode) -> (start : Int) -> (count : Nat) -> IO ()
  removeTailFrom _      _     Z         = pure ()
  removeTailFrom parent start (S k) = do
    node <- childNodeAt parent (start + cast k)
    ignore (removeChild parent node)
    assert_total (removeTailFrom parent start k)

  ||| Place one `next` child under `c`. A key present on both sides reuses
  ||| the live node: byte-identical => move it untouched; changed => move
  ||| it AND diff in place (preserving identity + listeners). A new/unkeyed
  ||| child renders fresh.
  placeKeyedChild :
       (c : DomNode) -> (keyed : List (String, DomNode))
    -> (prevByKey : List (String, HExpr)) -> (reusable : List String)
    -> HExpr -> IO ()
  placeKeyedChild c keyed prevByKey reusable h = case hexprKey h of
    Just k =>
      case lookupKeyed k keyed of
        Just node =>
          if elem k reusable
            then ignore (appendChild c node)   -- byte-identical: move only
            else do
              ignore (appendChild c node)       -- moved into next-order...
              case lookup k prevByKey of        -- ...then diffed in place
                Just oldH => assert_total (diffNode c node oldH h)
                Nothing   => pure ()
        Nothing => do fresh <- renderDom h; appendChild c fresh
    Nothing => do fresh <- renderDom h; appendChild c fresh

  ||| Diff `container`'s direct element children by `data-key`. Keys on
  ||| both sides reuse their live node (moved into `next` order via
  ||| `appendChild`); byte-identical keys are left untouched, changed keys
  ||| are diffed in place; new keys render fresh; vanished keys are
  ||| removed. Reused nodes keep DOM state (focus, scroll, input value)
  ||| and their listeners.
  export
  reconcileKeyedChildren :
       (container : DomNode)
    -> (previous : List HExpr)
    -> (next : List HExpr)
    -> IO ()
  reconcileKeyedChildren container previous next = do
    n     <- childCount container
    keyed <- keyedSnapshot container 0 n
    let reusable  = reusableKeys previous next
    let prevByKey = mapMaybe (\h => map (\k => (k, h)) (hexprKey h)) previous
    traverse_ (placeKeyedChild container keyed prevByKey reusable) next
    traverse_ (removeStaleKeyed container reusable) keyed

--------------------------------------------------------------------------------
-- Reconcile entry point (the only function the runtime calls to apply a
-- render). Defined after the diff machinery so it needs no forward refs.
--------------------------------------------------------------------------------

||| In-place reconcile (D1+D2): the host holds exactly one rendered root
||| child (from `mountInto` or a prior reconcile). Locate it and diff it
||| against `next` via `diffNode`, mutating the live tree in place so DOM
||| identity (focus, scroll, input value, listeners) survives. Falls back
||| to a fresh mount if the host is unexpectedly empty.
|||
||| No focus/scroll capture-restore bracket: in-place diff never destroys
||| the focused node, so re-focusing is unnecessary and would stomp a
||| selection the user changed mid-update. The Day-1 blow-and-rebuild
||| bracket retires exactly as its comments predicted.
export
reconcile : (host : DomNode) -> (previous : HExpr) -> (next : HExpr) -> IO ()
reconcile host previous next = do
  n <- childNodeCount host
  if n <= 0
    then mountInto host next
    else do
      root <- childNodeAt host 0
      diffNode host root previous next
