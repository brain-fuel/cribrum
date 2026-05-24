||| Cribrum.AA.Promote — generic factor for the structural-AA promotions.
|||
||| Plan §P4.2: "Generalise the `So`-trick. Factor the bool-predicate + `So`-
||| lifting + `All` over `walkNodes` pattern into `Cribrum.AA.Promote` so
||| adding a structural rule is ~10 lines."
|||
||| The shape recurring across `Cribrum.AA.Typed` for the per-node rules
||| (img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend,
||| button-name, link-name):
|||
|||   - a bool predicate `<rule>OkBool : HExpr -> Bool` that says whether
|||     *one* node satisfies the rule (non-target nodes return True);
|||   - a node-level proposition `<rule>HereOk h = So (<rule>OkBool h)`;
|||   - a whole-tree proposition `<rule>sAllOk h = All <rule>HereOk (walkNodes h)`;
|||   - a decision over the walked node list, propagating refutation through
|||     `All`'s head and tail.
|||
||| Only the bool predicate differs per rule. This module exports `AllNodesOk`,
||| `decAllNodesOk`, and the helper `allNodesOk` (bool projection) so each
||| rule's typed wrapper is a thin pointer to its bool predicate.
module Cribrum.AA.Promote

import Data.List.Quantifiers
import Data.So
import Cribrum.Node

%default total

||| Pre-order walk of every node in `h` (root included). Re-exported here so
||| callers in `Cribrum.AA.Typed` can keep using a single source.
public export
walkNodes : HExpr -> List HExpr
walkNodes h@(Text _)         = [h]
walkNodes h@(Comment _)      = [h]
walkNodes h@(Element _ _ cs) =
  h :: assert_total (concatMap walkNodes cs)

||| Single-node proposition lifted from a bool predicate. Witness is `Oh` —
||| informational structure is sacrificed to keep each rule's boilerplate
||| short.
public export
NodeOk : (HExpr -> Bool) -> HExpr -> Type
NodeOk pred h = So (pred h)

public export
decNodeOk : (pred : HExpr -> Bool) -> (h : HExpr) -> Dec (NodeOk pred h)
decNodeOk pred h = decSo (pred h)

||| Decision over an explicit node list. Re-used by `decAllNodesOk` once
||| `walkNodes` produces the list.
public export
decAllListOk : (pred : HExpr -> Bool) -> (xs : List HExpr) -> Dec (All (NodeOk pred) xs)
decAllListOk _    []        = Yes []
decAllListOk pred (x :: xs) = case decNodeOk pred x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllListOk pred xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

||| Whole-tree proposition: every node satisfies `pred`.
public export
AllNodesOk : (HExpr -> Bool) -> HExpr -> Type
AllNodesOk pred h = All (NodeOk pred) (walkNodes h)

||| Whole-tree decision.
public export
decAllNodesOk : (pred : HExpr -> Bool) -> (h : HExpr) -> Dec (AllNodesOk pred h)
decAllNodesOk pred h = decAllListOk pred (walkNodes h)

||| Bool projection of the whole-tree decision. Ergonomic for tests that
||| just want a truth-valued answer without destructuring the witness.
public export
allNodesOk : (HExpr -> Bool) -> HExpr -> Bool
allNodesOk pred h = case decAllNodesOk pred h of
  Yes _ => True
  No  _ => False
