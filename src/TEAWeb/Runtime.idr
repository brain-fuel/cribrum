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

%foreign "browser:lambda:(cb,w)=>{ window.__cribrumDispatch = (cbId, ev) => { window.__cribrumValue = (ev && ev.target && typeof ev.target.value === 'string') ? ev.target.value : ''; cb(cbId)(ev)(w); }; }"
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
-- Runtime state.
--------------------------------------------------------------------------------

||| Mutable state held across renders. The `IORef` is private to the
||| runtime; nothing outside `TEAWeb.Runtime` touches it.
record RuntimeState (m : Type) (ms : Type) where
  constructor MkRuntimeState
  current  : m
  tree     : HExpr
  handlers : List (String, Event -> IO ms)
  host     : DomNode

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
||| `mountInto`.
export
mount : Program model msg -> (hostId : String) -> IO ()
mount prog hostId = do
  host <- getElementById hostId
  let (initialModel, initialCmd) = prog.init
  let initialView                = prog.view initialModel
  -- Mount the initial DOM.
  mountInto host (tree initialView)
  -- Initialise the mutable state. `model` lives here, never escapes.
  stateRef <-
    newIORef
      (MkRuntimeState
        initialModel
        (tree initialView)
        (handlers initialView)
        host)
  -- Install the single global dispatcher. The closure captures
  -- `stateRef`, so all future renders update the handler table by
  -- writing to the IORef — no FFI required after this point.
  installDispatch $ \cbId, event => do
    state <- readIORef stateRef
    case lookupHandler cbId (handlers state) of
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
            state.host)
        runCmd newCmd
  -- Run the initial Cmd after the first render.
  runCmd initialCmd
