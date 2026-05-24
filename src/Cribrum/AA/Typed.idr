||| Phase 4 — type-level promotion of the structural AA rules.
|||
||| Per plan.dj §Phase 4: "the essence of Phase 2 applied to Phase 3." Reuse
||| the indexed-proposition + `Dec` machinery from Phase 2 on the
||| `Structural` rules from Phase 3. A rule **graduates** from finding to
||| proposition; it is never defined twice.
|||
||| Promotions wired (10 of 10 Structural rules in the catalog):
|||   per-node, So + All over walkNodes:
|||     img-alt, anchor-href, iframe-title, label-for-control,
|||     fieldset-legend, button-name, link-name
|||   root-only, So:
|||     document-lang
|||   whole-tree bool + So:
|||     heading-no-skip, duplicate-id
|||
||| Every Structural rule from `Cribrum.AA.Catalog` is now promoted; the
||| data-interpreter factor (plan §P4.2 "generalise the So-trick") lands
||| in `Cribrum.AA.Promote` if/when the boilerplate becomes annoying
||| enough to compress.
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

--------------------------------------------------------------------------------
-- anchor-href: identical shape to img-alt; second rule wired in to
-- demonstrate that each Structural rule generalises mechanically.
--------------------------------------------------------------------------------

hasHrefAttr : List HAttr -> Bool
hasHrefAttr []                       = False
hasHrefAttr (MkHAttr "href" _ :: _)  = True
hasHrefAttr (_                :: xs) = hasHrefAttr xs

public export
anchorOkBool : HExpr -> Bool
anchorOkBool (Element "a" attrs _) = hasHrefAttr attrs
anchorOkBool _                     = True

public export
AnchorHereOk : HExpr -> Type
AnchorHereOk h = So (anchorOkBool h)

public export
decAnchorHereOk : (h : HExpr) -> Dec (AnchorHereOk h)
decAnchorHereOk h = decSo (anchorOkBool h)

public export
AnchorsAllOk : HExpr -> Type
AnchorsAllOk h = All AnchorHereOk (walkNodes h)

decAllAnchor : (xs : List HExpr) -> Dec (All AnchorHereOk xs)
decAllAnchor []        = Yes []
decAllAnchor (x :: xs) = case decAnchorHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllAnchor xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decAnchorsAllOk : (h : HExpr) -> Dec (AnchorsAllOk h)
decAnchorsAllOk h = decAllAnchor (walkNodes h)

public export
anchorsAllOk : HExpr -> Bool
anchorsAllOk h = case decAnchorsAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- iframe-title: identical shape.
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

public export
iframeOkBool : HExpr -> Bool
iframeOkBool (Element "iframe" attrs _) = hasNonEmptyAttr "title" attrs
iframeOkBool _                          = True

public export
IframeHereOk : HExpr -> Type
IframeHereOk h = So (iframeOkBool h)

public export
decIframeHereOk : (h : HExpr) -> Dec (IframeHereOk h)
decIframeHereOk h = decSo (iframeOkBool h)

public export
IframesAllOk : HExpr -> Type
IframesAllOk h = All IframeHereOk (walkNodes h)

decAllIframe : (xs : List HExpr) -> Dec (All IframeHereOk xs)
decAllIframe []        = Yes []
decAllIframe (x :: xs) = case decIframeHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllIframe xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decIframesAllOk : (h : HExpr) -> Dec (IframesAllOk h)
decIframesAllOk h = decAllIframe (walkNodes h)

public export
iframesAllOk : HExpr -> Bool
iframesAllOk h = case decIframesAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- label-for-control: a `<label>` is OK iff it has a non-empty `for`
-- attribute OR contains at least one form-control descendant. The
-- check looks at the label's direct children — implicit-label pattern.
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
LabelHereOk h = So (labelOkBool h)

public export
decLabelHereOk : (h : HExpr) -> Dec (LabelHereOk h)
decLabelHereOk h = decSo (labelOkBool h)

public export
LabelsAllOk : HExpr -> Type
LabelsAllOk h = All LabelHereOk (walkNodes h)

decAllLabel : (xs : List HExpr) -> Dec (All LabelHereOk xs)
decAllLabel []        = Yes []
decAllLabel (x :: xs) = case decLabelHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllLabel xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decLabelsAllOk : (h : HExpr) -> Dec (LabelsAllOk h)
decLabelsAllOk h = decAllLabel (walkNodes h)

public export
labelsAllOk : HExpr -> Bool
labelsAllOk h = case decLabelsAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- button-name: a `<button>` is OK iff it has an accessible name.
--------------------------------------------------------------------------------

collectButtonText : HExpr -> String
collectButtonText (Text s)         = s
collectButtonText (Comment _)      = ""
collectButtonText (Element _ _ cs) = concatMap (assert_total collectButtonText) cs

hasButtonAccessibleName : List HAttr -> List HExpr -> Bool
hasButtonAccessibleName attrs cs =
     hasNonEmptyAttr "aria-label" attrs
  || hasNonEmptyAttr "title"      attrs
  || (let txt = concatMap collectButtonText cs
       in case unpack txt of
            []   => False
            xs   => not (all (== ' ') xs))

public export
buttonOkBool : HExpr -> Bool
buttonOkBool (Element "button" attrs cs) = hasButtonAccessibleName attrs cs
buttonOkBool _                            = True

public export
ButtonHereOk : HExpr -> Type
ButtonHereOk h = So (buttonOkBool h)

public export
decButtonHereOk : (h : HExpr) -> Dec (ButtonHereOk h)
decButtonHereOk h = decSo (buttonOkBool h)

public export
ButtonsAllOk : HExpr -> Type
ButtonsAllOk h = All ButtonHereOk (walkNodes h)

decAllButton : (xs : List HExpr) -> Dec (All ButtonHereOk xs)
decAllButton []        = Yes []
decAllButton (x :: xs) = case decButtonHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllButton xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decButtonsAllOk : (h : HExpr) -> Dec (ButtonsAllOk h)
decButtonsAllOk h = decAllButton (walkNodes h)

public export
buttonsAllOk : HExpr -> Bool
buttonsAllOk h = case decButtonsAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- fieldset-legend: a `<fieldset>` is OK iff it contains a `<legend>`
-- child (its accessible name). Mirrors label-for-control.
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
FieldsetHereOk h = So (fieldsetOkBool h)

public export
decFieldsetHereOk : (h : HExpr) -> Dec (FieldsetHereOk h)
decFieldsetHereOk h = decSo (fieldsetOkBool h)

public export
FieldsetsAllOk : HExpr -> Type
FieldsetsAllOk h = All FieldsetHereOk (walkNodes h)

decAllFieldset : (xs : List HExpr) -> Dec (All FieldsetHereOk xs)
decAllFieldset []        = Yes []
decAllFieldset (x :: xs) = case decFieldsetHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllFieldset xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decFieldsetsAllOk : (h : HExpr) -> Dec (FieldsetsAllOk h)
decFieldsetsAllOk h = decAllFieldset (walkNodes h)

public export
fieldsetsAllOk : HExpr -> Bool
fieldsetsAllOk h = case decFieldsetsAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- link-name: a `<a href>` is OK iff it has an accessible name (text content,
-- aria-label, or title). Mirrors button-name.
--------------------------------------------------------------------------------

collectLinkText : HExpr -> String
collectLinkText (Text s)         = s
collectLinkText (Comment _)      = ""
collectLinkText (Element _ _ cs) = concatMap (assert_total collectLinkText) cs

hasLinkAccessibleName : List HAttr -> List HExpr -> Bool
hasLinkAccessibleName attrs cs =
     hasNonEmptyAttr "aria-label" attrs
  || hasNonEmptyAttr "title"      attrs
  || (let txt = concatMap collectLinkText cs
       in case unpack txt of
            []   => False
            xs   => not (all (== ' ') xs))

||| Anchors without `href` are skipped (the structural anchor-href rule
||| handles them); only `<a href>` requires an accessible name here.
public export
linkOkBool : HExpr -> Bool
linkOkBool (Element "a" attrs cs) =
  if hasHrefAttr attrs
    then hasLinkAccessibleName attrs cs
    else True
linkOkBool _                       = True

public export
LinkHereOk : HExpr -> Type
LinkHereOk h = So (linkOkBool h)

public export
decLinkHereOk : (h : HExpr) -> Dec (LinkHereOk h)
decLinkHereOk h = decSo (linkOkBool h)

public export
LinksAllOk : HExpr -> Type
LinksAllOk h = All LinkHereOk (walkNodes h)

decAllLink : (xs : List HExpr) -> Dec (All LinkHereOk xs)
decAllLink []        = Yes []
decAllLink (x :: xs) = case decLinkHereOk x of
  No  contraHead => No (\(headOk :: _) => contraHead headOk)
  Yes headOk     => case decAllLink xs of
    No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
    Yes tailOk     => Yes (headOk :: tailOk)

public export
decLinksAllOk : (h : HExpr) -> Dec (LinksAllOk h)
decLinksAllOk h = decAllLink (walkNodes h)

public export
linksAllOk : HExpr -> Bool
linksAllOk h = case decLinksAllOk h of
  Yes _ => True
  No  _ => False

--------------------------------------------------------------------------------
-- document-lang: ROOT-only rule. The proposition fires only when the
-- top-level node is `<html>`; descendant `<html>` nodes (illegal anyway
-- under Phase-2 validity) are ignored here.
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
-- heading-no-skip: WHOLE-TREE rule. The list of heading levels (pre-order)
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
