||| Type-level smoke check for `Cribrum.Render.Dom`.
|||
||| The DOM renderer's FFI primitives only execute under the Idris 2 JS
||| backend; this chez-driven test suite cannot invoke them. What it can
||| (and must) do is hold the *signatures* steady — these are exactly
||| what TEAWeb's Phase T runtime is going to depend on. If a primitive
||| changes name or shape, this module breaks at compile time and the
||| failure surfaces in the regular test run before TEAWeb ever sees it.
module Test.Cribrum.Render.Dom

import Hedgehog
import Cribrum.Node
import Cribrum.Render.Dom

%default total

--------------------------------------------------------------------------------
-- Signature witnesses. Type-checking these is the entire test.
--------------------------------------------------------------------------------

sig_createElement      : String -> IO DomNode
sig_createElement      = createElement

sig_createTextNode     : String -> IO DomNode
sig_createTextNode     = createTextNode

sig_createComment      : String -> IO DomNode
sig_createComment      = createComment

sig_setAttribute       : DomNode -> String -> String -> IO ()
sig_setAttribute       = setAttribute

sig_removeAttribute    : DomNode -> String -> IO ()
sig_removeAttribute    = removeAttribute

sig_addEventListener   : DomNode -> String -> String -> IO ()
sig_addEventListener   = addEventListener

sig_appendChild        : DomNode -> DomNode -> IO ()
sig_appendChild        = appendChild

sig_replaceChild       : DomNode -> DomNode -> DomNode -> IO ()
sig_replaceChild       = replaceChild

sig_getElementById     : String -> IO DomNode
sig_getElementById     = getElementById

sig_clearChildren      : DomNode -> IO ()
sig_clearChildren      = clearChildren

sig_renderDom          : HExpr -> IO DomNode
sig_renderDom          = renderDom

sig_reconcile          : DomNode -> HExpr -> HExpr -> IO ()
sig_reconcile          = reconcile

sig_mountInto          : DomNode -> HExpr -> IO ()
sig_mountInto          = mountInto

--------------------------------------------------------------------------------
-- Hedgehog wrapper so the runner counts this as a passing test group.
--------------------------------------------------------------------------------

||| The module compiles and exposes every primitive TEAWeb needs.
||| Side-effect: re-typechecks every `sig_*` witness on every test run.
export
ext_dom_signatures_compile : Property
ext_dom_signatures_compile = withTests 1 . property $ do
  -- Pattern-match a few key witnesses to make the dependency explicit.
  let _ = sig_renderDom
      _ = sig_reconcile
      _ = sig_addEventListener
      _ = sig_mountInto
  success

export
group : Group
group = MkGroup "Cribrum.Render.Dom"
  [ ("ext_dom_signatures_compile", ext_dom_signatures_compile)
  ]
