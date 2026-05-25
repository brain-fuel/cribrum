||| Phase 4 — type-level promotion of the structural AA rules.
|||
||| Per plan.dj §Phase 4: "the essence of Phase 2 applied to Phase 3." Reuse
||| the indexed-proposition + `Dec` machinery from Phase 2 on the
||| `Structural` rules from Phase 3. A rule **graduates** from finding to
||| proposition; it is never defined twice.
|||
||| Promotions wired (10 of 10 Structural rules in the catalog):
|||   per-node, So + All over walkNodes — via `Cribrum.AA.Promote`:
|||     img-alt, anchor-href, iframe-title, label-for-control,
|||     fieldset-legend, button-name, link-name
|||   root-only, So:
|||     document-lang
|||   whole-tree bool + So:
|||     heading-no-skip, duplicate-id
|||
||| Every Structural rule from `Cribrum.AA.Catalog` is now promoted. Adding
||| a new per-node rule is ~5 lines: a `<rule>OkBool` predicate plus four
||| `public export` aliases through `Cribrum.AA.Promote`.
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
import Data.So
import Cribrum.Node
import public Cribrum.AA.Promote

%default total

--------------------------------------------------------------------------------
-- Shared attribute / accessible-name helpers.
--------------------------------------------------------------------------------

isNonEmptyStr : AttrValue -> Bool
isNonEmptyStr (Str s) = case unpack s of
  [] => False
  cs => not (all (== ' ') cs)
isNonEmptyStr _       = False

hasNonEmptyAttr : String -> List HAttr -> Bool
hasNonEmptyAttr _    []                        = False
hasNonEmptyAttr name (MkHAttr n v :: rest) =
  (n == name && isNonEmptyStr v) || hasNonEmptyAttr name rest

hasAltAttr : List HAttr -> Bool
hasAltAttr []                       = False
hasAltAttr (MkHAttr "alt" _ :: _)   = True
hasAltAttr (_                :: xs) = hasAltAttr xs

hasHrefAttr : List HAttr -> Bool
hasHrefAttr []                       = False
hasHrefAttr (MkHAttr "href" _ :: _)  = True
hasHrefAttr (_                :: xs) = hasHrefAttr xs

collectText : HExpr -> String
collectText (Text s)         = s
collectText (Comment _)      = ""
collectText (Element _ _ cs) = concatMap (assert_total collectText) cs

hasAccessibleName : List HAttr -> List HExpr -> Bool
hasAccessibleName attrs cs =
     hasNonEmptyAttr "aria-label" attrs
  || hasNonEmptyAttr "title"      attrs
  || (let txt = concatMap collectText cs
       in case unpack txt of
            []   => False
            xs   => not (all (== ' ') xs))

--------------------------------------------------------------------------------
-- img-alt: per-node, via Promote.
--------------------------------------------------------------------------------

||| `True` iff this single node satisfies the img-alt rule. Non-img nodes
||| trivially satisfy.
public export
imgOkBool : HExpr -> Bool
imgOkBool (Element "img" attrs _) = hasAltAttr attrs
imgOkBool _                       = True

public export
ImgHereOk : HExpr -> Type
ImgHereOk = NodeOk imgOkBool

public export
decImgHereOk : (h : HExpr) -> Dec (ImgHereOk h)
decImgHereOk = decNodeOk imgOkBool

public export
ImgsAllOk : HExpr -> Type
ImgsAllOk = AllNodesOk imgOkBool

public export
decImgsAllOk : (h : HExpr) -> Dec (ImgsAllOk h)
decImgsAllOk = decAllNodesOk imgOkBool

public export
imgsAllOk : HExpr -> Bool
imgsAllOk = allNodesOk imgOkBool

--------------------------------------------------------------------------------
-- anchor-href: per-node.
--------------------------------------------------------------------------------

public export
anchorOkBool : HExpr -> Bool
anchorOkBool (Element "a" attrs _) = hasHrefAttr attrs
anchorOkBool _                     = True

public export
AnchorHereOk : HExpr -> Type
AnchorHereOk = NodeOk anchorOkBool

public export
decAnchorHereOk : (h : HExpr) -> Dec (AnchorHereOk h)
decAnchorHereOk = decNodeOk anchorOkBool

public export
AnchorsAllOk : HExpr -> Type
AnchorsAllOk = AllNodesOk anchorOkBool

public export
decAnchorsAllOk : (h : HExpr) -> Dec (AnchorsAllOk h)
decAnchorsAllOk = decAllNodesOk anchorOkBool

public export
anchorsAllOk : HExpr -> Bool
anchorsAllOk = allNodesOk anchorOkBool

--------------------------------------------------------------------------------
-- iframe-title: per-node.
--------------------------------------------------------------------------------

public export
iframeOkBool : HExpr -> Bool
iframeOkBool (Element "iframe" attrs _) = hasNonEmptyAttr "title" attrs
iframeOkBool _                          = True

public export
IframeHereOk : HExpr -> Type
IframeHereOk = NodeOk iframeOkBool

public export
decIframeHereOk : (h : HExpr) -> Dec (IframeHereOk h)
decIframeHereOk = decNodeOk iframeOkBool

public export
IframesAllOk : HExpr -> Type
IframesAllOk = AllNodesOk iframeOkBool

public export
decIframesAllOk : (h : HExpr) -> Dec (IframesAllOk h)
decIframesAllOk = decAllNodesOk iframeOkBool

public export
iframesAllOk : HExpr -> Bool
iframesAllOk = allNodesOk iframeOkBool

--------------------------------------------------------------------------------
-- label-for-control: per-node. A `<label>` is OK iff it has a non-empty
-- `for` attribute OR contains at least one form-control descendant
-- (implicit-label pattern).
--------------------------------------------------------------------------------

labelContainsControl : List HExpr -> Bool
labelContainsControl []        = False
labelContainsControl (c :: cs) = case c of
  Element t _ _ =>
    elem t ["input", "select", "textarea", "button"
           , "output", "meter", "progress"]
      || labelContainsControl cs
  _             => labelContainsControl cs

public export
labelOkBool : HExpr -> Bool
labelOkBool (Element "label" attrs cs) =
  hasNonEmptyAttr "for" attrs || labelContainsControl cs
labelOkBool _                          = True

public export
LabelHereOk : HExpr -> Type
LabelHereOk = NodeOk labelOkBool

public export
decLabelHereOk : (h : HExpr) -> Dec (LabelHereOk h)
decLabelHereOk = decNodeOk labelOkBool

public export
LabelsAllOk : HExpr -> Type
LabelsAllOk = AllNodesOk labelOkBool

public export
decLabelsAllOk : (h : HExpr) -> Dec (LabelsAllOk h)
decLabelsAllOk = decAllNodesOk labelOkBool

public export
labelsAllOk : HExpr -> Bool
labelsAllOk = allNodesOk labelOkBool

--------------------------------------------------------------------------------
-- fieldset-legend: per-node. A `<fieldset>` is OK iff it contains a
-- `<legend>` direct child.
--------------------------------------------------------------------------------

fieldsetHasLegend : List HExpr -> Bool
fieldsetHasLegend []        = False
fieldsetHasLegend (c :: cs) = case c of
  Element "legend" _ _ => True
  _                    => fieldsetHasLegend cs

public export
fieldsetOkBool : HExpr -> Bool
fieldsetOkBool (Element "fieldset" _ cs) = fieldsetHasLegend cs
fieldsetOkBool _                          = True

public export
FieldsetHereOk : HExpr -> Type
FieldsetHereOk = NodeOk fieldsetOkBool

public export
decFieldsetHereOk : (h : HExpr) -> Dec (FieldsetHereOk h)
decFieldsetHereOk = decNodeOk fieldsetOkBool

public export
FieldsetsAllOk : HExpr -> Type
FieldsetsAllOk = AllNodesOk fieldsetOkBool

public export
decFieldsetsAllOk : (h : HExpr) -> Dec (FieldsetsAllOk h)
decFieldsetsAllOk = decAllNodesOk fieldsetOkBool

public export
fieldsetsAllOk : HExpr -> Bool
fieldsetsAllOk = allNodesOk fieldsetOkBool

--------------------------------------------------------------------------------
-- button-name: per-node. A `<button>` is OK iff it has an accessible name.
--------------------------------------------------------------------------------

public export
buttonOkBool : HExpr -> Bool
buttonOkBool (Element "button" attrs cs) = hasAccessibleName attrs cs
buttonOkBool _                            = True

public export
ButtonHereOk : HExpr -> Type
ButtonHereOk = NodeOk buttonOkBool

public export
decButtonHereOk : (h : HExpr) -> Dec (ButtonHereOk h)
decButtonHereOk = decNodeOk buttonOkBool

public export
ButtonsAllOk : HExpr -> Type
ButtonsAllOk = AllNodesOk buttonOkBool

public export
decButtonsAllOk : (h : HExpr) -> Dec (ButtonsAllOk h)
decButtonsAllOk = decAllNodesOk buttonOkBool

public export
buttonsAllOk : HExpr -> Bool
buttonsAllOk = allNodesOk buttonOkBool

--------------------------------------------------------------------------------
-- link-name: per-node. A `<a href>` is OK iff it has an accessible name.
-- Anchors without `href` are skipped (the structural anchor-href rule
-- handles those).
--------------------------------------------------------------------------------

public export
linkOkBool : HExpr -> Bool
linkOkBool (Element "a" attrs cs) =
  if hasHrefAttr attrs
    then hasAccessibleName attrs cs
    else True
linkOkBool _                       = True

public export
LinkHereOk : HExpr -> Type
LinkHereOk = NodeOk linkOkBool

public export
decLinkHereOk : (h : HExpr) -> Dec (LinkHereOk h)
decLinkHereOk = decNodeOk linkOkBool

public export
LinksAllOk : HExpr -> Type
LinksAllOk = AllNodesOk linkOkBool

public export
decLinksAllOk : (h : HExpr) -> Dec (LinksAllOk h)
decLinksAllOk = decAllNodesOk linkOkBool

public export
linksAllOk : HExpr -> Bool
linksAllOk = allNodesOk linkOkBool

--------------------------------------------------------------------------------
-- document-lang: ROOT-only rule. Proposition fires only when the top-level
-- node is `<html>`; descendant `<html>` nodes (illegal anyway under
-- Phase-2 validity) are ignored here.
--------------------------------------------------------------------------------

public export
documentLangOkBool : HExpr -> Bool
documentLangOkBool (Element "html" attrs _) = hasNonEmptyAttr "lang" attrs
documentLangOkBool _                        = True

public export
DocumentLangOk : HExpr -> Type
DocumentLangOk h = So (documentLangOkBool h)

public export
decDocumentLangOk : (h : HExpr) -> Dec (DocumentLangOk h)
decDocumentLangOk h = decSo (documentLangOkBool h)

public export
documentLangOk : HExpr -> Bool
documentLangOk h = case decDocumentLangOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- heading-no-skip: WHOLE-TREE rule. List of heading levels (pre-order)
-- must not skip — e.g. h1 -> h3 is rejected; h1 -> h2 -> h2 is fine.
--------------------------------------------------------------------------------

isHeadingTag : String -> Maybe Nat
isHeadingTag "h1" = Just 1
isHeadingTag "h2" = Just 2
isHeadingTag "h3" = Just 3
isHeadingTag "h4" = Just 4
isHeadingTag "h5" = Just 5
isHeadingTag "h6" = Just 6
isHeadingTag _    = Nothing

collectHeadingLevels : HExpr -> List Nat
collectHeadingLevels (Element t _ cs) = case isHeadingTag t of
  Just lvl => lvl :: assert_total (concatMap collectHeadingLevels cs)
  Nothing  => assert_total (concatMap collectHeadingLevels cs)
collectHeadingLevels _ = []

noSkip : Maybe Nat -> List Nat -> Bool
noSkip _        []        = True
noSkip Nothing  (l :: ls) = noSkip (Just l) ls
noSkip (Just p) (l :: ls) =
  if l > S p then False else noSkip (Just l) ls

public export
headingNoSkipOkBool : HExpr -> Bool
headingNoSkipOkBool h = noSkip Nothing (collectHeadingLevels h)

public export
HeadingNoSkipOk : HExpr -> Type
HeadingNoSkipOk h = So (headingNoSkipOkBool h)

public export
decHeadingNoSkipOk : (h : HExpr) -> Dec (HeadingNoSkipOk h)
decHeadingNoSkipOk h = decSo (headingNoSkipOkBool h)

public export
headingNoSkipOk : HExpr -> Bool
headingNoSkipOk h = case decHeadingNoSkipOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- duplicate-id: WHOLE-TREE rule. No two elements may share the same
-- `id` attribute value.
--------------------------------------------------------------------------------

idValue : List HAttr -> Maybe String
idValue []                              = Nothing
idValue (MkHAttr "id" (Str s) :: _)     = Just s
idValue (_ :: rest)                     = idValue rest

collectIdValues : HExpr -> List String
collectIdValues (Element _ attrs cs) = case idValue attrs of
  Just v  => v :: assert_total (concatMap collectIdValues cs)
  Nothing => assert_total (concatMap collectIdValues cs)
collectIdValues _ = []

allUnique : List String -> Bool
allUnique []        = True
allUnique (x :: xs) = if elem x xs then False else allUnique xs

public export
duplicateIdOkBool : HExpr -> Bool
duplicateIdOkBool h = allUnique (collectIdValues h)

public export
DuplicateIdOk : HExpr -> Type
DuplicateIdOk h = So (duplicateIdOkBool h)

public export
decDuplicateIdOk : (h : HExpr) -> Dec (DuplicateIdOk h)
decDuplicateIdOk h = decSo (duplicateIdOkBool h)

public export
duplicateIdOk : HExpr -> Bool
duplicateIdOk h = case decDuplicateIdOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- Partitioning audit predicate (plan.dj §P3.3).
--
-- `isTypedPromoted ruleId` is `True` iff this module exposes an
-- `IsAA_<rule>` proposition (with `Dec` decider) for that catalog id.
-- The partition test in `Test.Cribrum.AA.Partition` asserts that this
-- predicate agrees with `confidence == Structural` across every entry
-- in `Cribrum.AA.Catalog.allRules` — every Structural rule has a typed
-- promotion, and no Heuristic/Runtime rule does.
--
-- Adding a new typed promotion: also add its catalog id here.
--------------------------------------------------------------------------------

public export
isTypedPromoted : String -> Bool
isTypedPromoted "img-alt"           = True
isTypedPromoted "anchor-href"       = True
isTypedPromoted "iframe-title"      = True
isTypedPromoted "label-for-control" = True
isTypedPromoted "fieldset-legend"   = True
isTypedPromoted "button-name"       = True
isTypedPromoted "link-name"         = True
isTypedPromoted "document-lang"     = True
isTypedPromoted "heading-no-skip"   = True
isTypedPromoted "duplicate-id"      = True
isTypedPromoted _                   = False
