||| Phase 4 spike — type-level promotion of one structural AA rule.
|||
||| Per plan.md §Phase 4: "the essence of Phase 2 applied to Phase 3." Reuse
||| the indexed-proposition + `Dec` machinery from Phase 2 on the
||| `Structural` rules from Phase 3. A rule **graduates** from finding to
||| proposition; it is never defined twice.
|||
||| Spike scope: the `img-alt` rule. Mechanics generalise to every Structural
||| rule in `Cribrum.AA.Catalog`; each one wires in independently.
|||
||| Convention: predicates are **functions returning `Type`** rather than
||| dedicated `data` declarations. This avoids the boilerplate of one
||| constructor + one refutation case per rule, and the decision procedure
||| reduces to a `Dec (So ...)` wrapped over the existing boolean check.
||| The cost is that the witness is `Oh` (uninformative) rather than a
||| structured term — fine here, since callers want "this tree is conformant"
||| not "this tree is conformant *because* of these pieces."
module Cribrum.AA.Typed

import Data.List
import Data.List.Quantifiers
import Data.So
import Cribrum.Node

%default total

--------------------------------------------------------------------------------
-- img-alt at one node.
--
-- Reuses `Data.So.decSo : (b : Bool) -> Dec (So b)` directly; the proposition
-- machinery for Bool-shaped rules already lives in base.
--------------------------------------------------------------------------------

hasAltAttr : List HAttr -> Bool
hasAltAttr []                       = False
hasAltAttr (MkHAttr "alt" _ :: _)   = True
hasAltAttr (_                :: xs) = hasAltAttr xs

||| `True` iff this single node satisfies the img-alt rule. Non-img nodes
||| trivially satisfy.
public export
imgOkBool : HExpr -> Bool
imgOkBool (Element "img" attrs _) = hasAltAttr attrs
imgOkBool _                       = True

||| Proposition: this node satisfies img-alt. Phase 4 callers carry the
||| witness in `(h : HExpr ** ImgHereOk h)`.
public export
ImgHereOk : HExpr -> Type
ImgHereOk h = So (imgOkBool h)

public export
decImgHereOk : (h : HExpr) -> Dec (ImgHereOk h)
decImgHereOk h = decSo (imgOkBool h)

--------------------------------------------------------------------------------
-- img-alt over the whole tree.
--------------------------------------------------------------------------------

||| Pre-order walk of every node in `h` (root included).
public export
walkNodes : HExpr -> List HExpr
walkNodes h@(Text _)         = [h]
walkNodes h@(Comment _)      = [h]
walkNodes h@(Element _ _ cs) =
  h :: assert_total (concatMap walkNodes cs)

||| Whole-tree proposition: every node satisfies the img-alt rule.
public export
ImgsAllOk : HExpr -> Type
ImgsAllOk h = All ImgHereOk (walkNodes h)

||| Decision over the walked node list.
decAllImg : (xs : List HExpr) -> Dec (All ImgHereOk xs)
decAllImg []        = Yes []
decAllImg (x :: xs) = case decImgHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllImg xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decImgsAllOk : (h : HExpr) -> Dec (ImgsAllOk h)
decImgsAllOk h = decAllImg (walkNodes h)

||| Bool projection for ergonomics. The proof, not this bool, is the value
||| Phase 4 callers should carry — but most call sites just want to assert
||| the precondition in tests.
public export
imgsAllOk : HExpr -> Bool
imgsAllOk h = case decImgsAllOk h of
  Yes _ => True
  No  _ => False
