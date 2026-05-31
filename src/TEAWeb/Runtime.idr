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
||| Full inventory interpreted: `Cmd None / Batch / Focus / Blur / Http /
||| Random / After / SendPort`, and every `Sub` leaf (`OnKeyDown /
||| OnKeyUp / OnAnimationFrame / Every / Port`) installed + diffed across
||| renders (see `diffSubs`). Async Cmds (Http / Random / After) round-
||| trip through one-shot `cmdHandlers` entries keyed by a fresh id.
module TEAWeb.Runtime

import Data.IORef
import Data.List
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

-- Each installer captures its teardown handle into a per-cbId registry
-- (`window.__cribrumSubHandles[cbId]`) so `prim__teardownSub` can later
-- remove the listener / clear the timer / cancel the frame loop. The
-- keydown/keyup variants must store the *listener fn reference* itself
-- (removeEventListener matches by identity); the rAF variant stores a
-- `cancelled` flag the loop checks before re-scheduling (a bare
-- cancelAnimationFrame races a frame already queued).

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ window.__cribrumSubHandles = window.__cribrumSubHandles || {}; const fn = (ev)=>{ window.__cribrumKey = (ev && typeof ev.key === 'string') ? ev.key : ''; if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, ev); } }; document.addEventListener('keydown', fn); window.__cribrumSubHandles[cbId] = { kind: 'keydown', fn: fn }; }"
prim__installSubKeyDown : String -> PrimIO ()

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ window.__cribrumSubHandles = window.__cribrumSubHandles || {}; const fn = (ev)=>{ window.__cribrumKey = (ev && typeof ev.key === 'string') ? ev.key : ''; if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, ev); } }; document.addEventListener('keyup', fn); window.__cribrumSubHandles[cbId] = { kind: 'keyup', fn: fn }; }"
prim__installSubKeyUp : String -> PrimIO ()

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ window.__cribrumSubHandles = window.__cribrumSubHandles || {}; const h = { kind: 'raf', cancelled: false, raf: 0 }; const step = (ts)=>{ if (h.cancelled) return; window.__cribrumTimestamp = Number(ts); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } if (!h.cancelled) { h.raf = requestAnimationFrame(step); } }; window.__cribrumSubHandles[cbId] = h; h.raf = requestAnimationFrame(step); }"
prim__installSubAnimationFrame : String -> PrimIO ()

%foreign "scheme:(lambda (_,_) 0)"
         "browser:lambda:(cbId,period)=>{ window.__cribrumSubHandles = window.__cribrumSubHandles || {}; const id = setInterval(()=>{ window.__cribrumTimestamp = Number(Date.now()); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }, period); window.__cribrumSubHandles[cbId] = { kind: 'interval', id: id }; }"
prim__installSubInterval : String -> Integer -> PrimIO ()

%foreign "scheme:(lambda (_,_) 0)"
         "browser:lambda:(cbId,portName)=>{ window.__cribrumSubHandles = window.__cribrumSubHandles || {}; window.__cribrumPorts = window.__cribrumPorts || {}; window.__cribrumPorts[portName] = (msg)=>{ window.__cribrumPortMsg = String(msg); if (typeof window.__cribrumDispatch === 'function') { window.__cribrumDispatch(cbId, {}); } }; window.__cribrumSubHandles[cbId] = { kind: 'port', portName: portName }; }"
prim__installSubPort : String -> String -> PrimIO ()

%foreign "scheme:(lambda (_) 0)"
         "browser:lambda:(cbId)=>{ const r = (window.__cribrumSubHandles || {})[cbId]; if (!r) return; if (r.kind === 'keydown') { document.removeEventListener('keydown', r.fn); } else if (r.kind === 'keyup') { document.removeEventListener('keyup', r.fn); } else if (r.kind === 'raf') { r.cancelled = true; if (r.raf) cancelAnimationFrame(r.raf); } else if (r.kind === 'interval') { clearInterval(r.id); } else if (r.kind === 'port') { if (window.__cribrumPorts) { delete window.__cribrumPorts[r.portName]; } } delete window.__cribrumSubHandles[cbId]; }"
prim__teardownSub : String -> PrimIO ()

installSubKeyDown : String -> IO ()
installSubKeyDown cb = primIO (prim__installSubKeyDown cb)

installSubKeyUp : String -> IO ()
installSubKeyUp cb = primIO (prim__installSubKeyUp cb)

installSubAnimationFrame : String -> IO ()
installSubAnimationFrame cb = primIO (prim__installSubAnimationFrame cb)

installSubInterval : String -> Integer -> IO ()
installSubInterval cb period = primIO (prim__installSubInterval cb period)

installSubPort : String -> String -> IO ()
installSubPort cb name = primIO (prim__installSubPort cb name)

||| Tear down the browser resource a leaf opened, keyed by callback id.
export
teardownSub : String -> IO ()
teardownSub cb = primIO (prim__teardownSub cb)

||| Walk a `Sub msg` tree, install one browser-side listener per leaf,
||| and build the matching `(callbackId, Event -> IO msg)` entries the
||| dispatcher consults to project each delivery back to `msg`.
|||
||| Each projection reads the right window slot (`__cribrumKey` for
||| keydown, `__cribrumTimestamp` for rAF / Every, `__cribrumPortMsg`
||| for Port) before applying the user's `String -> msg` / `Double ->
||| msg` callback.
|||
||| The dispatch-table entry for one leaf: its callback id paired with a
||| projection closure that reads the right window slot and applies the
||| leaf's `... -> msg`. PURE (no FFI) — rebuilt from the latest leaf set
||| every render so projection changes are picked up without reinstalling
||| the browser listener. `None` / `Batch` have no entry.
export
leafEntry : Sub msg -> Maybe (String, Event -> IO msg)
leafEntry None                   = Nothing
leafEntry (Batch _)              = Nothing
leafEntry (OnKeyDown cb proj)    = Just (cb, \ev => map proj currentEventKey)
leafEntry (OnKeyUp cb proj)      = Just (cb, \ev => map proj currentEventKey)
leafEntry (OnAnimationFrame cb proj) = Just (cb, \ev => map proj currentEventTimestamp)
leafEntry (Every cb _ proj)      = Just (cb, \ev => map proj currentEventTimestamp)
leafEntry (Port cb _ proj)       = Just (cb, \ev => map proj currentEventPortMsg)

||| Build the dispatch table for a whole leaf list (drops structural
||| nodes). Pure projection-building; no browser side effects.
export
leafEntries : List (Sub msg) -> List (String, Event -> IO msg)
leafEntries = mapMaybe leafEntry

||| Install the browser-side resource for each leaf (FFI only — the
||| dispatch entries are built separately by `leafEntries`). Called with
||| the *flattened* leaf list; `None` / `Batch` never reach here.
export
installSubLeaf : Sub msg -> IO ()
installSubLeaf None                   = pure ()
installSubLeaf (Batch _)              = pure ()
installSubLeaf (OnKeyDown cb _)       = installSubKeyDown cb
installSubLeaf (OnKeyUp cb _)         = installSubKeyUp cb
installSubLeaf (OnAnimationFrame cb _) = installSubAnimationFrame cb
installSubLeaf (Every cb period _)    = installSubInterval cb period
installSubLeaf (Port cb portName _)   = installSubPort cb portName

||| Install every leaf in a list. Used at mount for the initial set and
||| in the dispatcher for the newly-appeared leaves of a re-evaluated
||| `subscriptions`.
export
installSubsLeaves : List (Sub msg) -> IO ()
installSubsLeaves = assert_total (traverse_ installSubLeaf)

--------------------------------------------------------------------------------
-- Runtime state.
--------------------------------------------------------------------------------

||| Mutable state held across renders. The `IORef` is private to the
||| runtime; nothing outside `TEAWeb.Runtime` touches it.
|||
||| `handlers` is rebuilt from `handlers nextView` each render so view
||| events stay in lockstep with the rendered tree. `subHandlers` is
||| installed once at mount and persists across renders — Sub leaves
||| don't get redrawn out from under their listeners. `cmdHandlers`
||| holds one-shot projection closures registered by async Cmds (Http,
||| After, Random); a delivery consumes its entry so the closure never
||| fires twice. `nextId` is a monotonic counter feeding fresh, never-
||| colliding callback ids for those async Cmds. The dispatch lookup
||| checks `handlers`, then `subHandlers`, then `cmdHandlers`.
record RuntimeState (m : Type) (ms : Type) where
  constructor MkRuntimeState
  current      : m
  tree         : HExpr
  handlers     : List (String, Event -> IO ms)
  subHandlers  : List (String, Event -> IO ms)
  ||| The flattened leaf set currently installed. Diffed against
  ||| `flatten (subscriptions newModel)` after each update so vanished
  ||| leaves are torn down and new ones installed. Holds whole leaves
  ||| (not just cbIds) because teardown needs the variant and reinstall
  ||| needs the projection.
  prevSubs     : List (Sub ms)
  cmdHandlers  : List (String, Event -> IO ms)
  nextId       : Integer
  host         : DomNode

--------------------------------------------------------------------------------
-- Cmd interpretation.
--
-- The synchronous leaves (Focus, Blur, SendPort) act immediately. The
-- async leaves (Http, After, Random) need a round-trip through the
-- dispatch loop to feed their resulting `msg` back into `update`: each
-- registers a one-shot projection closure in `cmdHandlers` under a
-- fresh callback id, then asks the FFI to deliver to that id when it
-- settles. `runCmd` is therefore parameterised over a `register` action
-- (adds a `(cbId, Event -> IO msg)` entry to the live state) and a
-- `freshId` action (returns a never-colliding callback id). `Batch`
-- recurses; `None` is inert.
--------------------------------------------------------------------------------

||| JSON-object string for a header list: `{"K":"V",...}`. Minimal
||| escaping (quotes + backslashes) is enough for header names/values.
jsonEscape : String -> String
jsonEscape s = pack (concatMap esc (unpack s))
  where
    esc : Char -> List Char
    esc '"'  = ['\\', '"']
    esc '\\' = ['\\', '\\']
    esc c    = [c]

headersJson : List HttpHeader -> String
headersJson hs =
  "{" ++ go True hs ++ "}"
  where
    go : Bool -> List HttpHeader -> String
    go _     []             = ""
    go first ((k, v) :: rest) =
      (if first then "" else ",")
        ++ "\"" ++ jsonEscape k ++ "\":\"" ++ jsonEscape v ++ "\""
        ++ go False rest

||| Run a single Cmd. `register cb fn` installs a one-shot projection;
||| `freshId` yields a unique callback id. Both are supplied by `mount`
||| (they close over the runtime `IORef`).
export
runCmdWith :
     (register : String -> (Event -> IO msg) -> IO ())
  -> (freshId  : IO String)
  -> Cmd msg
  -> IO ()
runCmdWith _   _    None              = pure ()
runCmdWith reg fid (Batch cmds)       =
  assert_total (traverse_ (runCmdWith reg fid) cmds)
runCmdWith _   _   (Focus id)         = focusElement id
runCmdWith _   _   (Blur  id)         = blurElement id
runCmdWith reg fid (Http m u hs b onResult) = do
  cb <- fid
  reg cb $ \_ => do
    err <- currentHttpErr
    if err == ""
      then do
        status <- currentHttpStatus
        body   <- currentHttpBody
        pure (onResult (HttpOk status body))
      else pure (onResult (HttpErr err))
  httpFetch cb (methodName m) u (headersJson hs) b
runCmdWith reg fid (Random lo hi onValue) = do
  cb <- fid
  reg cb $ \_ => map onValue (randomInt lo hi)
  dispatchNow cb
runCmdWith reg fid (After delayMs onElapsed) = do
  cb <- fid
  reg cb $ \_ => pure onElapsed
  setTimeoutDispatch cb delayMs
runCmdWith _   _   (SendPort portName payload) = sendPort portName payload

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

||| Drop the one-shot Cmd-handler entry keyed by `cbId` from a runtime
||| state. Pinned to `RuntimeState` so the record update's field
||| resolves without an inline type annotation (`model`/`msg` are not
||| in scope as names inside `mount`'s body).
dropCmdHandler : String -> RuntimeState m ms -> RuntimeState m ms
dropCmdHandler cbId st =
  { cmdHandlers $= filter (\kv => fst kv /= cbId) } st

||| Push a one-shot Cmd-handler entry. Pinned to `RuntimeState` for the
||| same reason as `dropCmdHandler`.
addCmdHandler : (String, Event -> IO ms) -> RuntimeState m ms -> RuntimeState m ms
addCmdHandler entry st = { cmdHandlers $= (entry ::) } st

||| Bump the fresh-id counter, returning the consumed id and the new
||| state.
bumpId : RuntimeState m ms -> (Integer, RuntimeState m ms)
bumpId st = (st.nextId, { nextId := st.nextId + 1 } st)

||| Mount a `Program` into the DOM element with the given id, starting
||| the dispatch loop. Total at the Idris side; impurity isolated to
||| the FFI calls inside `runCmdWith`, `reconcile`, `installDispatch`,
||| `mountInto`, and the Sub-leaf installers.
export
mount : Program model msg -> (hostId : String) -> IO ()
mount prog hostId = do
  host <- getElementById hostId
  let (initialModel, initialCmd) = prog.init
  let initialView                = prog.view initialModel
  -- Mount the initial DOM.
  mountInto host (tree initialView)
  -- Install the initial subscription set. The flattened leaf list is
  -- kept in `prevSubs` so each later render can diff `subscriptions
  -- newModel` against it (install new leaves, tear down vanished ones).
  -- `subHandlers` holds the projection table, rebuilt from the latest
  -- leaves every render so projection changes are picked up for free.
  let initialSubs = flatten (prog.subscriptions initialModel)
  installSubsLeaves initialSubs
  let subHs = leafEntries initialSubs
  -- Initialise the mutable state. `model` lives here, never escapes.
  stateRef <-
    newIORef
      (MkRuntimeState
        initialModel
        (tree initialView)
        (handlers initialView)
        subHs
        initialSubs
        []          -- cmdHandlers: no async Cmds in flight yet
        0           -- nextId
        host)
  -- `register` adds a one-shot projection closure under `cbId`; the
  -- dispatcher removes it once it fires. `freshId` mints a unique
  -- callback id from the monotonic counter (prefixed so it can never
  -- collide with an app-supplied view/sub callback id).
  let register : String -> (Event -> IO msg) -> IO ()
      register cbId fn = do
        st <- readIORef stateRef
        writeIORef stateRef (addCmdHandler (cbId, fn) st)
  let freshId : IO String
      freshId = do
        st <- readIORef stateRef
        let (n, st') = bumpId st
        writeIORef stateRef st'
        pure ("__cmd-" ++ show n)
  let run : Cmd msg -> IO ()
      run = runCmdWith register freshId
  -- Install the single global dispatcher. The closure captures
  -- `stateRef`, so all future renders update the handler table by
  -- writing to the IORef — no FFI required after this point.
  installDispatch $ \cbId, event => do
    state <- readIORef stateRef
    -- View handlers, then sub handlers, then one-shot cmd handlers.
    let table = handlers state ++ state.subHandlers ++ state.cmdHandlers
    case lookupHandler cbId table of
      Nothing => pure ()
      Just fn => do
        msg <- fn event
        -- Re-read: `fn` (an async Cmd projection) may itself have run
        -- effects, but more importantly we must drop a consumed
        -- one-shot cmd handler so it never fires twice.
        s0 <- readIORef stateRef
        let s1 = dropCmdHandler cbId s0
        let (newModel, newCmd) = prog.update msg s1.current
        let nextView           = prog.view newModel
        let nextTree           = tree nextView
        -- Skip reconcile when the view tree is unchanged. Day-1
        -- "blow-and-rebuild" otherwise nukes DOM-resident state
        -- (input values, scroll position, ...) on every dispatch,
        -- *including* dispatches whose Cmd is the only observable
        -- effect (Focus, Blur). Structural HExpr equality is a cheap
        -- O(tree) check; the keyed-children diff that arrives in
        -- Day-2 subsumes this guard.
        let prevTree = RuntimeState.tree s1
        let dirty = nextTree /= prevTree
        when dirty (reconcile s1.host prevTree nextTree)
        -- Re-evaluate subscriptions for the new model and diff against
        -- the currently-installed leaves. Tear down vanished leaves
        -- FIRST (so a same-cbId leaf whose params changed clears its old
        -- browser resource before the reinstall opens the new one), then
        -- install the newly-appeared leaves. `subHandlers` is rebuilt
        -- wholesale from the next leaf set so survivors keep firing with
        -- their current projection and dropped leaves stop being looked
        -- up.
        let nextSubs           = flatten (prog.subscriptions newModel)
        let (toInst, toTear)   = diffSubs s1.prevSubs nextSubs
        traverse_ teardownSub toTear
        installSubsLeaves toInst
        let nextSubHandlers    = leafEntries nextSubs
        writeIORef
          stateRef
          (MkRuntimeState
            newModel
            nextTree
            (handlers nextView)
            nextSubHandlers
            nextSubs
            s1.cmdHandlers
            s1.nextId
            s1.host)
        run newCmd
  -- Run the initial Cmd after the first render.
  run initialCmd

||| Mount an `AccessibleProgram` — a program whose `view` returns an
||| `AccessibleView msg`, i.e. whose every render is statically known to be
||| valid, accessible HTML. The runtime loop is identical to `mount`'s; the
||| only difference is the entry type, which guarantees at the boundary that
||| no non-accessible view can reach the DOM. We project the accessible
||| program down to a plain `Program` (forgetting the now-discharged proof)
||| and reuse `mount` verbatim.
export
mountAccessible : AccessibleProgram model msg -> (hostId : String) -> IO ()
mountAccessible ap hostId = mount (forgetAccessible ap) hostId
