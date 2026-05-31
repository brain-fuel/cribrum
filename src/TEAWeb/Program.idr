||| TEAWeb.Program — the Elm-style Program record (Phase T3).
|||
||| A `Program model msg` packages everything an app supplies into the
||| runtime: an initial model + first Cmd, an `update` function that
||| produces the next model + Cmd from a message, a `view` function
||| that renders the current model as a `View msg`, and a
||| `subscriptions` function that declares passive event streams.
|||
||| Per the locked decision, `view`'s return type is `View msg` (a pair
||| of HExpr + handler table). Once Phase 2 + Phase 4 land, the
||| codomain will sharpen to a proof-carrying
||| `(h : HExpr ** IsValidHtml h × StructuralAA h)` view; the field
||| name and arity stay the same.
module TEAWeb.Program

import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Html
import TEAWeb.Html.Accessible

%default total

public export
record Program (model : Type) (msg : Type) where
  constructor MkProgram
  init          : (model, Cmd msg)
  update        : msg -> model -> (model, Cmd msg)
  view          : model -> View msg
  subscriptions : model -> Sub msg

||| The accessible-codomain variant of `Program`: `view` returns an
||| `AccessibleView msg` instead of a bare `View msg`, so every state the
||| app can render is statically known to be valid, accessible HTML (its
||| tree carries the discharged `IsValidHtml × StructuralAA` witnesses).
|||
||| This realises the plan.dj §Phase T1 sharpening — "`view`'s return type
||| sharpens to `(h : HExpr ** IsValidHtml h × StructuralAA h)` once Phase 4
||| lands" — at the program boundary. The view function must thread its
||| renders through `decAccessibleView` (the only sanctioned constructor of
||| `AccessibleView`), so a non-accessible render is unrepresentable here:
||| it cannot produce the witness, hence cannot build the value the field
||| demands.
|||
||| `mountAccessible` (in `TEAWeb.Runtime`) consumes this record by
||| projecting each `AccessibleView` back to its underlying `View msg`.
public export
record AccessibleProgram (model : Type) (msg : Type) where
  constructor MkAccessibleProgram
  init          : (model, Cmd msg)
  update        : msg -> model -> (model, Cmd msg)
  view          : model -> AccessibleView msg
  subscriptions : model -> Sub msg

||| Forget the accessibility codomain: turn an `AccessibleProgram` into the
||| ordinary `Program` the standard runtime loop understands by projecting
||| every `AccessibleView` to its underlying `View msg`. The proof has
||| already gated construction; mounting only needs the tree + handlers.
public export
forgetAccessible : AccessibleProgram model msg -> Program model msg
forgetAccessible ap =
  MkProgram ap.init ap.update (\m => toView (ap.view m)) ap.subscriptions
