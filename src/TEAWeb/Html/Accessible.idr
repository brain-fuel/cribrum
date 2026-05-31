||| TEAWeb.Html.Accessible — `StructuralAA` in the view codomain (Phase T1
||| post-Phase-4 follow-on).
|||
||| Per `plan.dj` §Phase T1: "The view function's return type sharpens to
||| `(h : HExpr ** IsValidHtml h × StructuralAA h)` once Phase 4 lands."
||| This module lands that codomain as a *typed boundary*: an
||| `AccessibleView msg` is a `View msg` whose underlying `HExpr` carries
||| both Phase-2 (`IsValidHtml`) and Phase-4 (`StructuralAA`) witnesses, so
||| a value of this type is statically known to be valid, accessible HTML.
|||
||| The witnesses are *discharged*, never hand-built: `decAccessibleView`
||| runs the view's tree through `viewSafe` (the Phase-2 content-model gate)
||| and then `decStructuralAA` (the Phase-4 conjunct decision procedure).
||| `StructuralAA` is treated as opaque — we obtain its witness from
||| `decStructuralAA` and never construct its tuple by hand, so this module
||| is insensitive to how many conjuncts `StructuralAA` has.
|||
||| Boundary discipline:
|||
||| - View-builders stay total and unproven (`TEAWeb.Html`, `…Html.Typed`).
||| - `decAccessibleView` is the single point where the proof obligation is
|||   discharged. A view that fails either gate is *rejected* with a located
|||   diagnostic; only a passing view yields the proof-carrying value.
||| - `mountAccessible` (in `TEAWeb.Runtime`) consumes an
|||   `AccessibleProgram` whose `view` returns `AccessibleView msg`, so a
|||   program that mounts through that entry point can only mount views that
|||   were proven accessible.
module TEAWeb.Html.Accessible

import Cribrum.Node
import Cribrum.Html.Valid
import Cribrum.Elaborate
import TEAWeb.Html

%default total

--------------------------------------------------------------------------------
-- Rejection of a view that fails the accessibility boundary.
--------------------------------------------------------------------------------

||| Why a `View msg` failed `decAccessibleView`. Either the Phase-2
||| content-model gate rejected the tree (`NotValidHtml`, carrying the
||| located reject from `viewSafe`), or the tree is valid HTML but fails a
||| Phase-4 structural-accessibility rule (`NotAccessible`, carrying the
||| stable rule id from `Cribrum.AA.Catalog` and the path-into-tree of the
||| offending node — exactly what `decStructuralAA` reports).
public export
data AccError : Type where
  ||| The view's tree is not valid HTML. Wraps the Phase-2 `ViewError`.
  NotValidHtml : ViewError -> AccError
  ||| The tree is valid HTML but fails a Phase-4 structural rule. `rule`
  ||| is the catalog rule id (e.g. `"img-alt"`, `"heading-no-skip"`);
  ||| `path` locates the offending node (`Just []` for the root-only
  ||| document-lang rule, `Nothing` for whole-tree rules).
  NotAccessible : (rule : String) -> (path : Maybe (List Nat)) -> AccError

public export
Show AccError where
  show (NotValidHtml ve)   = show ve
  show (NotAccessible r p) =
    "view fails structural accessibility (" ++ r ++ ")" ++ explainRule r
      ++ case p of
           Nothing   => " — affecting the whole view"
           Just []   => " at the view root"
           Just pth  => " at child path " ++ show pth

--------------------------------------------------------------------------------
-- The proof-carrying view.
--------------------------------------------------------------------------------

||| A `View msg` whose underlying `HExpr` is statically known to be valid,
||| accessible HTML. `tree` is the same plain `HExpr` that mounts (no second
||| tree — the single-IR invariant from plan.dj §Governing principle holds);
||| `valid` and `accessible` are the discharged Phase-2 / Phase-4 witnesses;
||| `handlers` is the msg-producing callback table carried through unchanged
||| from the source `View msg`.
public export
record AccessibleView (msg : Type) where
  constructor MkAccessibleView
  tree       : HExpr
  valid      : IsValidHtml tree
  accessible : StructuralAA tree
  handlers   : List (String, Event -> IO msg)

||| Project an `AccessibleView msg` back to a plain `View msg` for the rest
||| of the TEAWeb pipeline (the runtime mounts the `tree` and installs the
||| `handlers`). The proof is erased at this boundary — it has already done
||| its job (gating construction).
public export
toView : AccessibleView msg -> View msg
toView av = MkView av.tree av.handlers

--------------------------------------------------------------------------------
-- The accessibility boundary.
--------------------------------------------------------------------------------

||| Discharge the accessibility obligation for a view. Runs the tree through
||| the Phase-2 content-model gate (`viewSafe`) and the Phase-4 structural
||| conjunct (`decStructuralAA`). On success, returns an `AccessibleView`
||| carrying both witnesses and the original handler table. On failure,
||| returns a located `AccError` naming the first violated rule.
|||
||| This is the *only* sanctioned way to obtain an `AccessibleView` from an
||| arbitrary view: the witnesses come from the decision procedures, never
||| from a hand-built tuple, so the module never depends on the internal
||| arity of `StructuralAA`.
public export
decAccessibleView : View msg -> Either AccError (AccessibleView msg)
decAccessibleView v@(MkView t hs) = case viewSafe v of
  Left ve                  => Left (NotValidHtml ve)
  Right ((t' ** valid), _) => case decStructuralAA t' of
    Left (rule, path) => Left (NotAccessible rule path)
    Right aa          => Right (MkAccessibleView t' valid aa hs)
