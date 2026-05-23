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

%default total

public export
record Program (model : Type) (msg : Type) where
  constructor MkProgram
  init          : (model, Cmd msg)
  update        : msg -> model -> (model, Cmd msg)
  view          : model -> View msg
  subscriptions : model -> Sub msg
