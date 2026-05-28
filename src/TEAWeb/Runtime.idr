||| TEAWeb.Runtime — interpreter loop, dispatcher install, Cmd
||| execution (Phase T3 + T4).
|||
||| The runtime's job is the perimeter: take the pure `Program model
||| msg` and bolt it to the browser. Three responsibilities:
|||
||| 1. *Mount.* Render the initial view, attach it to a host element,
|||    register the initial handler table.
||| 2. *Dispatch.* Install a single global function
|||    `window.__cribrumDispatch(cbId, event)` that the JS shim in
|||    `Render.Dom.addEventListener` calls when an event fires. The
|||    dispatcher looks the cbId up in the current handler table, runs
|||    `update`, reconciles the DOM, and stores the new state.
||| 3. *Cmd interpretation.* Walk the `Cmd msg` returned by `update`
|||    and execute each leaf via FFI.
|||
||| The interpreter loop lives in Idris (a tail-recursive function
||| driven by `IORef` state); only the FFI leaves cross the boundary.
||| `update` and `view` never see `IO`.
|||
||| MVP slice: `Cmd None / Batch / Focus / Blur` interpreted; `Sub` is
||| `None`-only at the moment (Counter+Focus demo needs no
||| subscriptions). Wiring for keyed-children diff + non-MVP Sub
||| variants arrives without touching the dispatch contract.
module TEAWeb.Runtime

import Data.IORef
import Cribrum.Node
import Cribrum.Render.Dom
import TEAWeb.Html
import TEAWeb.Event
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Program

%default total

--------------------------------------------------------------------------------
-- FFI: install the global dispatcher; element focus / blur.
--------------------------------------------------------------------------------

%foreign "browser:lambda:(cb,w)=>{ window.__cribrumDispatch = (cbId, ev) => { window.__cribrumValue = (ev && ev.target && typeof ev.target.value === 'string') ? ev.target.value : ''; window.__cribrumKey = (ev && typeof ev.key === 'string') ? ev.key : ''; cb(cbId)(ev)(w); }; }"
prim__installDispatch : (String -> Event -> IO ()) -> PrimIO ()

||| Install `window.__cribrumDispatch`. Called once by `mount`; future
||| renders update the handler table inside the closure, not via FFI.
installDispatch : (String -> Event -> IO ()) -> IO ()
installDispatch f = primIO (prim__installDispatch f)

%foreign "browser:lambda:(id)=>{ const el = document.getElementById(id); if (el && typeof el.focus==='function') el.focus(); }"
prim__focusElement : String -> PrimIO ()

%foreign "browser:lambda:(id)=>{ const el = document.getElementById(id); if (el && typeof el.blur==='function') el.blur(); }"
prim__blurElement : String -> PrimIO ()

focusElement : ElementId -> IO ()
focusElement id = primIO (prim__focusElement id)

blurElement : ElementId -> IO ()
blurElement id = primIO (prim__blurElement id)

--------------------------------------------------------------------------------
-- Sub-leaf installers. Each `Sub` leaf opens its own browser event
-- source (document keydown, requestAnimationFrame, setInterval, named
-- port slot) and routes deliveries through `window.__cribrumDispatch`.
-- For non-Event payloads (rAF timestamp, interval timestamp, port
-- message) the installer stashes the value into a window slot before
-- calling dispatch — the leaf's handler closure then pulls it back
-- out via `currentEventTimestamp` / `currentEventPortMsg`.
--
-- Chez stubs are no-ops so the runtime type-checks at the chez
-- backend even though the JS backend is the only one that runs the
-- code in anger.
--------------------------------------------------------------------------------

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ document.addEventListener('keydown', (ev)=>{ window.__cribrumKey = (ev && typeof ev.key === 'string') ? ev.key : ''; if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, ev); } }); }"
prim__installSubKeyDown : String -> PrimIO ()

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ const step = (ts)=>{ window.__cribrumTimestamp = Number(ts); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } requestAnimationFrame(step); }; requestAnimationFrame(step); }"
prim__installSubAnimationFrame : String -> PrimIO ()

%foreign "scheme:(lambda (_,_) 0)"
         "browser:lambda:(cbId,period)=>{ setInterval(()=>{ window.__cribrumTimestamp = Number(Date.now()); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }, period); }"
prim__installSubInterval : String -> Integer -> PrimIO ()

%foreign "scheme:(lambda (_,_) 0)"
         "browser:lambda:(cbId,portName)=>{ window.__cribrumPorts = window.__cribrumPorts || {}; window.__cribrumPorts[portName] = (msg)=>{ window.__cribrumPortMsg = String(msg); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }; }"
prim__installSubPort : String -> String -> PrimIO ()

installSubKeyDown : String -> IO ()
installSubKeyDown cb = primIO (prim__installSubKeyDown cb)

installSubAnimationFrame : String -> IO ()
installSubAnimationFrame cb = primIO (prim__installSubAnimationFrame cb)

installSubInterval : String -> Integer -> IO ()
installSubInterval cb period = primIO (prim__installSubInterval cb period)

installSubPort : String -> String -> IO ()
installSubPort cb name = primIO (prim__installSubPort cb name)

||| Walk a `Sub msg` tree, install one browser-side listener per leaf,
||| and build the matching `(callbackId, Event -> IO msg)` entries the
||| dispatcher consults to project each delivery back to `msg`.
|||
||| Each projection reads the right window slot (`__cribrumKey` for
||| keydown, `__cribrumTimestamp` for rAF / Every, `__cribrumPortMsg`
||| for Port) before applying the user's `String -> msg` / `Double ->
||| msg` callback.
|||
||| MVP-grade: install fires once at mount. Sub-tree diff across
||| renders (add/remove listeners as `subscriptions model` changes)
||| arrives with keyed-children reconcile; until then `subscriptions`
||| is effectively read once at startup. Leaves the existing
||| `None`-only counter demo wholly unaffected.
export
installSubs : Sub msg -> IO (List (String, Event -> IO msg))
installSubs None         = pure []
installSubs (Batch subs) =
  assert_total
    (foldlM
       (\acc, s => do
          entries <- installSubs s
          pure (acc ++ entries))
       []
       subs)
installSubs (OnKeyDown cb proj) = do
  installSubKeyDown cb
  pure [(cb, \ev => map proj currentEventKey)]
installSubs (OnAnimationFrame cb proj) = do
  installSubAnimationFrame cb
  pure [(cb, \ev => map proj currentEventTimestamp)]
installSubs (Every cb period proj) = do
  installSubInterval cb period
  pure [(cb, \ev => map proj currentEventTimestamp)]
installSubs (Port cb portName proj) = do
  installSubPort cb portName
  pure [(cb, \ev => map proj currentEventPortMsg)]

--------------------------------------------------------------------------------
-- Runtime state.
--------------------------------------------------------------------------------

||| Mutable state held across renders. The `IORef` is private to the
||| runtime; nothing outside `TEAWeb.Runtime` touches it.
|||
||| `handlers` is rebuilt from `handlers nextView` each render so view
||| events stay in lockstep with the rendered tree. `subHandlers` is
||| installed once at mount and persists across renders — Sub leaves
||| don't get redrawn out from under their listeners. The dispatch
||| lookup checks `handlers` first, then `subHandlers`.
record RuntimeState (m : Type) (ms : Type) where
  constructor MkRuntimeState
  current      : m
  tree         : HExpr
  handlers     : List (String, Event -> IO ms)
  subHandlers  : List (String, Event -> IO ms)
  host         : DomNode

--------------------------------------------------------------------------------
-- Cmd interpretation. Total recursion via `assert_total` on the Batch
-- case — `Cmd` is a finite tree by construction.
--------------------------------------------------------------------------------

||| Run a single Cmd by performing its side effect. Recurses through
||| `Batch`.
export
runCmd : Cmd msg -> IO ()
runCmd None         = pure ()
runCmd (Batch cmds) = assert_total (traverse_ runCmd cmds)
runCmd (Focus id)   = focusElement id
runCmd (Blur  id)   = blurElement id

--------------------------------------------------------------------------------
-- Mount + dispatch loop.
--------------------------------------------------------------------------------

||| Look the callback id up in the current handler table. Returns
||| Nothing if not found (silently drop — keeps stale-DOM-event races
||| from crashing the loop).
lookupHandler :
     String
  -> List (String, Event -> IO msg)
  -> Maybe (Event -> IO msg)
lookupHandler _   []                = Nothing
lookupHandler key ((k, fn) :: rest) =
  if k == key then Just fn else lookupHandler key rest

||| Mount a `Program` into the DOM element with the given id, starting
||| the dispatch loop. Total at the Idris side; impurity isolated to
||| the FFI calls inside `runCmd`, `reconcile`, `installDispatch`,
||| `mountInto`, and the Sub-leaf installers.
export
mount : Program model msg -> (hostId : String) -> IO ()
mount prog hostId = do
  host <- getElementById hostId
  let (initialModel, initialCmd) = prog.init
  let initialView                = prog.view initialModel
  -- Mount the initial DOM.
  mountInto host (tree initialView)
  -- Install the initial subscription set; the resulting (cbId,
  -- handler) entries persist across renders in `subHandlers` so the
  -- dispatcher can find them on Sub-driven deliveries even after
  -- view-handler refreshes.
  subHs <- installSubs (prog.subscriptions initialModel)
  -- Initialise the mutable state. `model` lives here, never escapes.
  stateRef <-
    newIORef
      (MkRuntimeState
        initialModel
        (tree initialView)
        (handlers initialView)
        subHs
        host)
  -- Install the single global dispatcher. The closure captures
  -- `stateRef`, so all future renders update the handler table by
  -- writing to the IORef — no FFI required after this point.
  installDispatch $ \cbId, event => do
    state <- readIORef stateRef
    case lookupHandler cbId (handlers state ++ state.subHandlers) of
      Nothing => pure ()
      Just fn => do
        msg <- fn event
        let (newModel, newCmd) = prog.update msg state.current
        let nextView           = prog.view newModel
        let nextTree           = tree nextView
        -- Skip reconcile when the view tree is unchanged. Day-1
        -- "blow-and-rebuild" otherwise nukes DOM-resident state
        -- (input values, scroll position, ...) on every dispatch,
        -- *including* dispatches whose Cmd is the only observable
        -- effect (Focus, Blur). Structural HExpr equality is a cheap
        -- O(tree) check; the keyed-children diff that arrives in
        -- Day-2 subsumes this guard.
        let dirty = nextTree /= state.tree
        when dirty (reconcile state.host state.tree nextTree)
        writeIORef
          stateRef
          (MkRuntimeState
            newModel
            nextTree
            (handlers nextView)
            state.subHandlers
            state.host)
        runCmd newCmd
  -- Run the initial Cmd after the first render.
  runCmd initialCmd
