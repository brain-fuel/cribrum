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
||| Per plan.dj §P3.2 the traversal engine is now a *data interpreter*: the
||| per-node pass walks a registry of `(Rule, NodeRuleCheck)` rows
||| (`nodeRuleImpls`) rather than calling each rule by name. The whole-tree
||| rules (heading-no-skip, duplicate-id, unique-main) stay as explicit
||| `++` terms in `checkAA` because their predicate operates on the entire
||| document, not on each visited node — three rows is small enough that
||| an explicit form is clearer than a registry of two competing scopes.
||| Adding a per-node rule is now one row in `nodeRuleImpls` paired with
||| its `Cribrum.AA.Catalog` `Rule` constant; partitioning audits
||| (plan §P3.3) become a simple set comparison over `map fst nodeRuleImpls`
||| plus the named whole-tree rules.
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

attrValue : String -> List HAttr -> Maybe String
attrValue _    []                          = Nothing
attrValue name (MkHAttr n (Str s) :: rest) =
  if n == name then Just s else attrValue name rest
attrValue name (_ :: rest)                 = attrValue name rest

altValue : List HAttr -> Maybe String
altValue = attrValue "alt"

||| `True` iff the (trimmed) attribute value exists and is non-empty.
hasNonEmptyAttr : String -> List HAttr -> Bool
hasNonEmptyAttr name attrs = case attrValue name attrs of
  Just v  => trim v /= ""
  Nothing => False

||| Walk a subtree and concatenate every text node's content. Used by
||| accessible-name checks (link-name, button-name).
collectText : HExpr -> String
collectText (Text s)         = s
collectText (Comment _)      = ""
collectText (Raw _)          = ""
collectText (Element _ _ cs) =
  concatMap (assert_total collectText) cs

||| `True` iff the subtree has any non-whitespace text or an
||| `aria-label` / `title` attribute on the root.
hasAccessibleName : List HAttr -> List HExpr -> Bool
hasAccessibleName attrs cs =
     hasNonEmptyAttr "aria-label" attrs
  || hasNonEmptyAttr "title"      attrs
  || trim (concatMap collectText cs) /= ""

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

||| `<html>` root must have a non-empty `lang`. STRUCTURAL.
||| Only fires at the root of the tree; the walker passes `True` for
||| the root call and `False` for descendants.
checkDocumentLang : Path -> Bool -> String -> List HAttr -> List Finding
checkDocumentLang p True "html" attrs =
  if hasNonEmptyAttr "lang" attrs
    then []
    else [MkFinding ruleDocumentLang p
            "<html> root missing non-empty lang attribute"]
checkDocumentLang _ _    _      _     = []

||| `<iframe>` must have a non-empty `title`. STRUCTURAL.
checkIframeTitle : Path -> String -> List HAttr -> List Finding
checkIframeTitle p "iframe" attrs =
  if hasNonEmptyAttr "title" attrs
    then []
    else [MkFinding ruleIframeTitle p
            "<iframe> missing non-empty title attribute"]
checkIframeTitle _ _ _ = []

||| `<label>` must have a `for` attribute OR contain at least one form
||| control (input / select / textarea / button) — the implicit-label
||| pattern. STRUCTURAL.
labelContainsControl : List HExpr -> Bool
labelContainsControl []        = False
labelContainsControl (c :: cs) = case c of
  Element t _ _ =>
    elem t ["input", "select", "textarea", "button", "output", "meter", "progress"]
      || labelContainsControl cs
  _             => labelContainsControl cs

checkLabelFor : Path -> String -> List HAttr -> List HExpr -> List Finding
checkLabelFor p "label" attrs cs =
  if hasNonEmptyAttr "for" attrs || labelContainsControl cs
    then []
    else [MkFinding ruleLabelFor p
            "<label> missing for attribute and contains no control"]
checkLabelFor _ _ _ _ = []

||| `<fieldset>` should contain a `<legend>` as its first applicable
||| descendant. STRUCTURAL (warning — accessible-name fallback exists).
fieldsetHasLegend : List HExpr -> Bool
fieldsetHasLegend []        = False
fieldsetHasLegend (c :: cs) = case c of
  Element "legend" _ _ => True
  _                    => fieldsetHasLegend cs

checkFieldsetLegend : Path -> String -> List HExpr -> List Finding
checkFieldsetLegend p "fieldset" cs =
  if fieldsetHasLegend cs
    then []
    else [MkFinding ruleFieldsetLegend p
            "<fieldset> missing <legend>"]
checkFieldsetLegend _ _ _ = []

||| `<a>` with an `href` must have an accessible name. STRUCTURAL.
checkLinkName : Path -> String -> List HAttr -> List HExpr -> List Finding
checkLinkName p "a" attrs cs =
  if hasAttr "href" attrs && not (hasAccessibleName attrs cs)
    then [MkFinding ruleLinkName p
            "<a href> has no accessible name (empty text + no aria-label/title)"]
    else []
checkLinkName _ _ _ _ = []

||| `<button>` must have an accessible name. STRUCTURAL.
checkButtonName : Path -> String -> List HAttr -> List HExpr -> List Finding
checkButtonName p "button" attrs cs =
  if hasAccessibleName attrs cs
    then []
    else [MkFinding ruleButtonName p
            "<button> has no accessible name (empty text + no aria-label/title)"]
checkButtonName _ _ _ _ = []

||| `<area>` with `href` must have an `alt` attribute (treated as a link
||| under WCAG 1.1.1). STRUCTURAL.
checkAreaAlt : Path -> String -> List HAttr -> List Finding
checkAreaAlt p "area" attrs =
  if hasAttr "href" attrs && not (hasAttr "alt" attrs)
    then [MkFinding ruleAreaAlt p "<area href> missing alt attribute"]
    else []
checkAreaAlt _ _    _     = []

||| `<a href="">` is ineffective per WCAG 2.4.4. STRUCTURAL warning.
checkLinkEmptyHref : Path -> String -> List HAttr -> List Finding
checkLinkEmptyHref p "a" attrs = case attrValue "href" attrs of
  Just ""  => [MkFinding ruleLinkEmptyHref p
                 "<a href=\"\"> is ineffective"]
  _        => []
checkLinkEmptyHref _ _ _ = []

||| `<meta http-equiv="refresh">` triggers an unsolicited timeout per
||| WCAG 2.2.1. STRUCTURAL warning.
checkMetaNoRefresh : Path -> String -> List HAttr -> List Finding
checkMetaNoRefresh p "meta" attrs = case attrValue "http-equiv" attrs of
  Just v  => if toLower v == "refresh"
               then [MkFinding ruleMetaNoRefresh p
                       "<meta http-equiv=\"refresh\"> auto-refreshes"]
               else []
  Nothing => []
checkMetaNoRefresh _ _ _ = []

||| `<track>` must declare a `kind` attribute. STRUCTURAL.
checkTrackKind : Path -> String -> List HAttr -> List Finding
checkTrackKind p "track" attrs =
  if hasAttr "kind" attrs
    then []
    else [MkFinding ruleTrackKind p "<track> missing kind attribute"]
checkTrackKind _ _ _ = []

||| `<details>` must contain a `<summary>` whose text is non-empty.
||| STRUCTURAL warning (a fallback default summary text is supplied by
||| the browser when missing, but it cannot describe the disclosure).
summaryHasText : List HExpr -> Bool
summaryHasText []        = False
summaryHasText (c :: cs) = case c of
  Element "summary" _ scs => trim (concatMap collectText scs) /= "" || summaryHasText cs
  _                        => summaryHasText cs

checkSummaryNotEmpty : Path -> String -> List HExpr -> List Finding
checkSummaryNotEmpty p "details" cs =
  if summaryHasText cs
    then []
    else [MkFinding ruleSummaryNotEmpty p
            "<details> missing non-empty <summary>"]
checkSummaryNotEmpty _ _ _ = []

||| `aria-label` should not literally duplicate the element's trimmed
||| visible text. HEURISTIC — the redundancy is a screen-reader smell,
||| not a hard violation.
checkAriaLabelRedundant : Path -> String -> List HAttr -> List HExpr -> List Finding
checkAriaLabelRedundant p _ attrs cs = case attrValue "aria-label" attrs of
  Just lbl =>
    let labelTrim = trim lbl
        textTrim  = trim (concatMap collectText cs)
    in if labelTrim /= "" && labelTrim == textTrim
         then [MkFinding ruleAriaLabelRedundant p
                 ("aria-label=\"" ++ lbl ++ "\" duplicates visible text")]
         else []
  Nothing => []

||| `tabindex` > 0 disrupts the natural focus order. HEURISTIC — a
||| positive tabindex may be intentional but is almost always wrong.
parseDigit : Char -> Maybe Nat
parseDigit '0' = Just 0
parseDigit '1' = Just 1
parseDigit '2' = Just 2
parseDigit '3' = Just 3
parseDigit '4' = Just 4
parseDigit '5' = Just 5
parseDigit '6' = Just 6
parseDigit '7' = Just 7
parseDigit '8' = Just 8
parseDigit '9' = Just 9
parseDigit _   = Nothing

parseNatString : String -> Maybe Nat
parseNatString s = case unpack s of
  []     => Nothing
  (c :: rest) => do
    d  <- parseDigit c
    ds <- traverse parseDigit rest
    pure (foldl (\acc, e => acc * 10 + e) d ds)

checkPositiveTabindex : Path -> List HAttr -> List Finding
checkPositiveTabindex p attrs = case attrValue "tabindex" attrs of
  Just v  => case parseNatString (trim v) of
    Just n  => if n > 0
                 then [MkFinding rulePositiveTabindex p
                         ("positive tabindex=" ++ show n
                            ++ " disrupts natural focus order")]
                 else []
    Nothing => []                      -- non-numeric (e.g. tabindex="-1") ignored
  Nothing => []

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
-- Duplicate-id (whole-tree).
--------------------------------------------------------------------------------

||| Collect (path, id-value) for every element in the tree carrying an
||| `id`. Pre-order.
collectIds : Path -> HExpr -> List (Path, String)
collectIds p (Element _ attrs cs) = case attrValue "id" attrs of
  Just v  =>
    let here = (p, v)
     in here :: childrenIds 0 cs
  Nothing => childrenIds 0 cs
  where
    childrenIds : Nat -> List HExpr -> List (Path, String)
    childrenIds _ [] = []
    childrenIds i (c :: rest) =
      assert_total (collectIds (p ++ [i]) c) ++ childrenIds (S i) rest
collectIds _ _ = []

||| Emit one finding per duplicate id occurrence (every occurrence after
||| the first emits, so callers see N-1 findings for N copies).
checkDuplicateIds : HExpr -> List Finding
checkDuplicateIds h = go [] (collectIds [] h)
  where
    go : List String -> List (Path, String) -> List Finding
    go _    []                = []
    go seen ((p, v) :: rest)  =
      if elem v seen
        then MkFinding ruleDuplicateId p ("duplicate id: " ++ v)
               :: go seen rest
        else go (v :: seen) rest

||| Count `<main>` elements in the whole tree (pre-order walk). Whole-tree
||| predicate: at most one `<main>` is allowed per WCAG SC 1.3.1.
mainCount : HExpr -> Nat
mainCount (Element "main" _ cs) = S (assert_total (sum (map mainCount cs)))
mainCount (Element _      _ cs) = assert_total (sum (map mainCount cs))
mainCount _                     = 0

||| Emit one finding when the tree contains more than one `<main>`. The
||| finding is whole-tree (path = []) because the violation is the
||| relation between multiple nodes rather than any single offending node.
checkUniqueMain : HExpr -> List Finding
checkUniqueMain h =
  if mainCount h <= 1
    then []
    else [MkFinding ruleUniqueMain []
            ("document contains " ++ show (mainCount h) ++ " <main> elements (expected at most 1)")]

--------------------------------------------------------------------------------
-- Data interpreter — per-node rule registry (plan §P3.2).
--------------------------------------------------------------------------------

||| Per-node rule signature. The walker invokes one of these at every
||| `Element` node, passing the path-into-tree, an `atRoot` flag (true only
||| for the document root call — used by `document-lang`), and the node
||| itself.
public export
NodeRuleCheck : Type
NodeRuleCheck = Path -> Bool -> HExpr -> List Finding

-- Adapters that lift the various existing per-rule signatures into
-- `NodeRuleCheck`. Each adapter destructures the `HExpr` once and forwards
-- the projection the rule cares about (attrs, attrs + children, children,
-- or attrs + root-flag).

pAttr : (Path -> String -> List HAttr -> List Finding) -> NodeRuleCheck
pAttr f p _ (Element t attrs _) = f p t attrs
pAttr _ _ _ _                   = []

pNode : (Path -> String -> List HAttr -> List HExpr -> List Finding) -> NodeRuleCheck
pNode f p _ (Element t attrs cs) = f p t attrs cs
pNode _ _ _ _                    = []

pCh : (Path -> String -> List HExpr -> List Finding) -> NodeRuleCheck
pCh f p _ (Element t _ cs) = f p t cs
pCh _ _ _ _                = []

pAttrsOnly : (Path -> List HAttr -> List Finding) -> NodeRuleCheck
pAttrsOnly f p _ (Element _ attrs _) = f p attrs
pAttrsOnly _ _ _ _                   = []

pRoot : (Path -> Bool -> String -> List HAttr -> List Finding) -> NodeRuleCheck
pRoot f p atRoot (Element t attrs _) = f p atRoot t attrs
pRoot _ _ _      _                   = []

||| The data interpreter's per-node rule table. Each row pairs a catalog
||| `Rule` (single source of truth) with its `NodeRuleCheck` impl. The
||| traversal engine (`nodeChecks` -> `walkNodes`) folds over this list,
||| so adding a per-node rule is one row.
|||
||| Order matches catalog order at the point of introduction; tests do not
||| assume a particular finding order (sort-before-compare when needed).
public export
nodeRuleImpls : List (Rule, NodeRuleCheck)
nodeRuleImpls =
  [ (ruleImgAlt,             pAttr      checkImgAlt)
  , (ruleAnchorHref,         pAttr      checkAnchorHref)
  , (ruleAltMeaningful,      pAttr      checkAltMeaningful)
  , (ruleDocumentLang,       pRoot      checkDocumentLang)
  , (ruleIframeTitle,        pAttr      checkIframeTitle)
  , (ruleLabelFor,           pNode      checkLabelFor)
  , (ruleFieldsetLegend,     pCh        checkFieldsetLegend)
  , (ruleLinkName,           pNode      checkLinkName)
  , (ruleButtonName,         pNode      checkButtonName)
  , (ruleAreaAlt,            pAttr      checkAreaAlt)
  , (ruleLinkEmptyHref,      pAttr      checkLinkEmptyHref)
  , (ruleMetaNoRefresh,      pAttr      checkMetaNoRefresh)
  , (ruleTrackKind,          pAttr      checkTrackKind)
  , (ruleSummaryNotEmpty,    pCh        checkSummaryNotEmpty)
  , (ruleAriaLabelRedundant, pNode      checkAriaLabelRedundant)
  , (rulePositiveTabindex,   pAttrsOnly checkPositiveTabindex)
  ]

||| Whole-tree rule signature. Invoked once on the document root by
||| `checkAA`; the predicate is responsible for its own descent.
public export
TreeRuleCheck : Type
TreeRuleCheck = HExpr -> List Finding

||| Whole-tree rule table — heading-no-skip, duplicate-id, unique-main
||| operate on the entire tree (the relation between nodes, not any single
||| node), so they live here rather than in `nodeRuleImpls`.
public export
treeRuleImpls : List (Rule, TreeRuleCheck)
treeRuleImpls =
  [ (ruleHeadingNoSkip, checkHeadingNoSkip)
  , (ruleDuplicateId,   checkDuplicateIds)
  , (ruleUniqueMain,    checkUniqueMain)
  ]

--------------------------------------------------------------------------------
-- Top-level traversal.
--------------------------------------------------------------------------------

||| Per-node checks (everything except heading-skip + duplicate-id + unique-
||| main, all three whole-tree). `atRoot` tags the root-call so `document-
||| lang` only fires on the document root. The catalog of rule impls lives
||| in `nodeRuleImpls`; this function is the data interpreter for it.
nodeChecks : Path -> Bool -> HExpr -> List Finding
nodeChecks p atRoot h@(Element _ _ _) =
  concatMap (\(_, f) => f p atRoot h) nodeRuleImpls
nodeChecks _ _ _ = []

||| Walk emitting per-node findings. Only the root call passes
||| `atRoot=True`; subtree walks pass `False`.
walkNodes : Path -> Bool -> HExpr -> List Finding
walkNodes p atRoot h@(Element _ _ cs) =
  nodeChecks p atRoot h ++ childWalk 0 cs
  where
    childWalk : Nat -> List HExpr -> List Finding
    childWalk _ [] = []
    childWalk i (c :: rest) =
      assert_total (walkNodes (p ++ [i]) False c) ++ childWalk (S i) rest
walkNodes _ _ _ = []

||| Full pass. Per plan.dj the function takes the `IsValidHtml` proof as a
||| precondition; we do not extract from it (the proof exists so the type
||| signature reflects the soundness story).
public export
checkAA : (h : HExpr) -> IsValidHtml h -> AAReport
checkAA h _ =
  walkNodes [] True h
    ++ checkHeadingNoSkip h
    ++ checkDuplicateIds h
    ++ checkUniqueMain h
  -- The whole-tree terms above mirror `treeRuleImpls`; they are kept in
  -- explicit form so the call-site reads at a glance and the existing
  -- mutation gate (which targets these literal `++ check...` terms)
  -- stays valid. `treeRuleImpls` is the canonical data view of the same
  -- three rules for partitioning audits.

||| Convenience: just the structurally-tagged findings (the Phase-4-promotable
||| set). Heuristic/runtime are still in the full report but separated here.
public export
structuralFindings : AAReport -> AAReport
structuralFindings = filter ((== Structural) . confidence . rule)

public export
heuristicFindings : AAReport -> AAReport
heuristicFindings = filter ((== Heuristic) . confidence . rule)
