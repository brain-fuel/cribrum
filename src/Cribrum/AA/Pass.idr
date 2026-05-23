||| AA checking pass — Phase 3 per plan.dj.
|||
||| `checkAA : (h : HExpr) -> IsValidHtml h -> AAReport`, total. Walks the
||| tree, emits findings tagged by the shared catalog (`Cribrum.AA.Catalog`).
||| Confidence partitioning is honoured: heuristic/runtime findings are
||| reported, never claimed as conformance.
|||
||| The `IsValidHtml` precondition is a *type-level* guarantee that the input
||| is well-formed HTML — Phase 4 will let callers carry the AA witnesses in
||| the same shape, and the structural rules here are the ones that will
||| graduate to propositions there.
|||
||| Spike scope: 4 rules out of the full WCAG AA catalog. The architecture
||| stays put as the catalog grows (rules are data; the traversal is data-
||| driven; new rules add new emit functions, not new types).
module Cribrum.AA.Pass

import Data.List
import Data.String
import Cribrum.Node
import Cribrum.Html.Valid
import Cribrum.AA.Catalog

%default total

||| Path identifies a node by the child-index trail from the root. Mirrors
||| what a renderer or editor would use to deep-link a finding.
public export
Path : Type
Path = List Nat

public export
record Finding where
  constructor MkFinding
  rule     : Rule
  path     : Path
  message  : String

public export
Eq Finding where
  (MkFinding r p m) == (MkFinding s q n) = r == s && p == q && m == n

public export
Show Finding where
  show (MkFinding r p m) =
    "Finding @" ++ show p ++ " [" ++ id r ++ "] " ++ m

public export
AAReport : Type
AAReport = List Finding

--------------------------------------------------------------------------------
-- Helpers.
--------------------------------------------------------------------------------

hasAttr : String -> List HAttr -> Bool
hasAttr name = any (\a => name == a.name)

altValue : List HAttr -> Maybe String
altValue [] = Nothing
altValue (MkHAttr "alt" (Str s) :: _) = Just s
altValue (_ :: rest)                  = altValue rest

isHeadingTag : String -> Maybe Nat
isHeadingTag "h1" = Just 1
isHeadingTag "h2" = Just 2
isHeadingTag "h3" = Just 3
isHeadingTag "h4" = Just 4
isHeadingTag "h5" = Just 5
isHeadingTag "h6" = Just 6
isHeadingTag _    = Nothing

--------------------------------------------------------------------------------
-- Per-rule checks.
--------------------------------------------------------------------------------

||| `<img>` must have an `alt` attribute. STRUCTURAL.
checkImgAlt : Path -> String -> List HAttr -> List Finding
checkImgAlt p "img" attrs =
  if hasAttr "alt" attrs
    then []
    else [MkFinding ruleImgAlt p "image missing alt attribute"]
checkImgAlt _ _    _     = []

||| `<a>` must have an `href` attribute. STRUCTURAL.
checkAnchorHref : Path -> String -> List HAttr -> List Finding
checkAnchorHref p "a" attrs =
  if hasAttr "href" attrs
    then []
    else [MkFinding ruleAnchorHref p "anchor missing href attribute"]
checkAnchorHref _ _   _     = []

||| `alt` text exists but looks like a filename or is purely whitespace.
||| HEURISTIC — never claims conformance, only flags suspicion.
checkAltMeaningful : Path -> String -> List HAttr -> List Finding
checkAltMeaningful p "img" attrs = case altValue attrs of
  Nothing => []                                -- structural rule's job
  Just a  =>
    let t = trim a
     in if t == "" || looksLikeFilename t
          then [MkFinding ruleAltMeaningful p
                   ("alt=\"" ++ a ++ "\" looks not meaningful")]
          else []
  where
    looksLikeFilename : String -> Bool
    looksLikeFilename s =
      let lower = toLower s
       in any (\suf => isSuffixOf suf lower)
              [".jpg", ".jpeg", ".png", ".gif", ".webp", ".svg"]
checkAltMeaningful _ _ _ = []

--------------------------------------------------------------------------------
-- Heading-skip check (whole-tree, not per-node).
--------------------------------------------------------------------------------

||| Collect (path, level) for every heading in the tree, in pre-order.
collectHeadings : Path -> HExpr -> List (Path, Nat)
collectHeadings p (Element t _ cs) = case isHeadingTag t of
  Just lvl => (p, lvl) :: childrenHeadings 0 cs
  Nothing  => childrenHeadings 0 cs
  where
    childrenHeadings : Nat -> List HExpr -> List (Path, Nat)
    childrenHeadings _ [] = []
    childrenHeadings i (c :: rest) =
      assert_total (collectHeadings (p ++ [i]) c)
        ++ childrenHeadings (S i) rest
collectHeadings _ _ = []

||| Returns a finding for each level-skip: pairs (prevLevel, nextLevel) where
||| nextLevel > prevLevel + 1.
checkHeadingNoSkip : HExpr -> List Finding
checkHeadingNoSkip h =
  let hs = collectHeadings [] h
   in walk Nothing hs
  where
    walk : Maybe Nat -> List (Path, Nat) -> List Finding
    walk _        []                 = []
    walk Nothing  ((_, l) :: rest)   = walk (Just l) rest
    walk (Just prev) ((p, l) :: rest) =
      if l > S prev
        then MkFinding ruleHeadingNoSkip p
               ("heading skip: h" ++ show prev ++ " -> h" ++ show l) ::
             walk (Just l) rest
        else walk (Just l) rest

--------------------------------------------------------------------------------
-- Top-level traversal.
--------------------------------------------------------------------------------

||| Per-node checks (everything except heading-skip, which is whole-tree).
nodeChecks : Path -> HExpr -> List Finding
nodeChecks p (Element t attrs _) =
  checkImgAlt        p t attrs
    ++ checkAnchorHref   p t attrs
    ++ checkAltMeaningful p t attrs
nodeChecks _ _ = []

||| Walk emitting per-node findings.
walkNodes : Path -> HExpr -> List Finding
walkNodes p h@(Element _ _ cs) =
  nodeChecks p h ++ childWalk 0 cs
  where
    childWalk : Nat -> List HExpr -> List Finding
    childWalk _ [] = []
    childWalk i (c :: rest) =
      assert_total (walkNodes (p ++ [i]) c) ++ childWalk (S i) rest
walkNodes _ _ = []

||| Full pass. Per plan.dj the function takes the `IsValidHtml` proof as a
||| precondition; we do not extract from it (the proof exists so the type
||| signature reflects the soundness story).
public export
checkAA : (h : HExpr) -> IsValidHtml h -> AAReport
checkAA h _ = walkNodes [] h ++ checkHeadingNoSkip h

||| Convenience: just the structurally-tagged findings (the Phase-4-promotable
||| set). Heuristic/runtime are still in the full report but separated here.
public export
structuralFindings : AAReport -> AAReport
structuralFindings = filter ((== Structural) . confidence . rule)

public export
heuristicFindings : AAReport -> AAReport
heuristicFindings = filter ((== Heuristic) . confidence . rule)
