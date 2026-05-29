||| `IsValidHtml` — Phase 2 per `plan.dj` §Phase 2.
|||
||| HTML well-formedness as an indexed proposition `IsValidHtml : HExpr -> Type`
||| with a total decision procedure `decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`.
||| Conformance is a *proof*, never a separate datatype: this is the single-type
||| invariant in action.
|||
||| The proposition decomposes per `plan.dj` §P2.2:
|||
|||     IsValidHtml (Element tag attrs cs)
|||       = IsKnownTag tag
|||       × All (AttrAllowedIn tag) attrs       -- per-element attribute model
|||       × All (ChildAllowedIn tag) cs         -- per-element content model
|||       × All  IsValidHtml         cs         -- recursion
|||
||| The catalog of permitted children + attributes lives in
||| `Cribrum.Html.Model`. `IsValidHtml`'s decision procedure is
||| data-driven: it interprets the catalog rather than hand-coding per-
||| element rules, so adding an element is a one-line catalog change.
|||
||| Located rejection (P2.3): `decideHtmlLocated` returns either the
||| `IsValidHtml` proof or a `LocatedReject` carrying the path-into-tree
||| plus a `RejectionClass` (`UnknownTag` / `DisallowedAttr` /
||| `IllegalChild` / `TextNotAllowedIn` / `BlockInPhrasing` /
||| `MalformedTable`).
module Cribrum.Html.Valid

import Data.List
import Data.List.Elem
import Data.List.Quantifiers
import Data.So
import Decidable.Equality
import Cribrum.Node
import Cribrum.Html.Category
import Cribrum.Html.Model

%default total

--------------------------------------------------------------------------------
-- Known-tag membership (sourced from Model.knownTagNames).
--------------------------------------------------------------------------------

||| Legacy alias: the list of every catalog tag. Kept exported so existing
||| call sites continue to compile.
public export
knownTags : List String
knownTags = knownTagNames

public export
IsKnownTag : String -> Type
IsKnownTag t = Elem t knownTags

public export
decKnownTag : (t : String) -> Dec (IsKnownTag t)
decKnownTag t = isElem t knownTags

--------------------------------------------------------------------------------
-- Attribute permission.
--------------------------------------------------------------------------------

||| `attr` is permitted on element `tag`. Decided by interpreting
||| `Model.isAllowedAttrName`.
public export
AttrAllowedIn : (tag : String) -> HAttr -> Type
AttrAllowedIn tag (MkHAttr name _) = So (isAllowedAttrName tag name)

public export
decAttrAllowed : (tag : String) -> (a : HAttr) -> Dec (AttrAllowedIn tag a)
decAttrAllowed tag (MkHAttr name _) = decSo (isAllowedAttrName tag name)

--------------------------------------------------------------------------------
-- Child placement.
--------------------------------------------------------------------------------

||| Implementation predicate: `True` iff `c` is permitted directly inside
||| element `parent` under `parent`'s content-model entry.
|||
||| * `NoChildren`        — nothing fits.
||| * `TextOnly`          — only `Text` / `Comment`; raw-text parents
|||                         (`<script>`, `<style>`, `<title>`, `<textarea>`)
|||                         additionally reject `Comment` — `<!--` inside
|||                         raw-text is character data, not a comment node.
||| * `AnyContent`        — anything (transparent / spec gap).
||| * `OnlyTags ts at`    — `Element t` iff `t ∈ ts`; `Text` iff `at`; comments OK.
||| * `OnlyCategories cs` — `Element t` iff `t`'s catalog categories overlap `cs`;
|||                         `Text` iff `Phrasing ∈ cs`; comments OK.
public export
childAllowedBool : (parent : String) -> HExpr -> Bool
childAllowedBool parent c = case childPolicyOf parent of
  NoChildren            => False
  TextOnly              => case c of
    Text _                => True
    Comment _             => not (isRawTextOf parent)
    Raw _                 => True
    Element _ _ _         => False
  AnyContent            => True
  OnlyTags ts allowText => case c of
    Text _                => allowText
    Comment _             => True
    Raw _                 => True
    Element t _ _         => elem t ts
  OnlyCategories cats   => case c of
    -- Text is phrasing content; phrasing ⊆ flow, so a parent that
    -- accepts Flow also accepts text. This subsumption mirrors the
    -- spec: any phrasing-content node is also flow content.
    Text _                => elem Phrasing cats || elem Flow cats
    Comment _             => True
    -- Raw passthrough is opaque content the author injected; it rides
    -- through any content model that admits children (only a void
    -- `NoChildren` parent rejects it). Placement is the author's call.
    Raw _                 => True
    Element t _ _         => anyOverlap (categoriesOf t) cats

||| `c` is permitted directly inside element `parent`.
public export
ChildAllowedIn : (parent : String) -> HExpr -> Type
ChildAllowedIn parent c = So (childAllowedBool parent c)

public export
decChildAllowed : (parent : String) -> (c : HExpr) -> Dec (ChildAllowedIn parent c)
decChildAllowed parent c = decSo (childAllowedBool parent c)

--------------------------------------------------------------------------------
-- IsValidHtml.
--------------------------------------------------------------------------------

||| Indexed proposition: structural well-formedness over HExpr.
|||
||| `ValidText` / `ValidComment`: leaves are trivially valid.
||| `ValidElement`: tag is known, every attribute is permitted on this
||| tag, every child is placement-legal under this tag's content model,
||| and every child is itself valid HTML.
public export
data IsValidHtml : HExpr -> Type where
  ValidText    : IsValidHtml (Text s)
  ValidComment : IsValidHtml (Comment s)
  ||| A `Raw` passthrough node is trivially valid: its content is an opaque
  ||| string the author asked to inject literally (Djot `=html`). We cannot
  ||| — and by design do not — inspect it, so the witness is unconditional.
  ||| This is the documented hole in the by-construction guarantee (see
  ||| `Cribrum.Node.Raw`).
  ValidRaw     : IsValidHtml (Raw s)
  ValidElement : (tagOk         : IsKnownTag tag)
              -> (attrsOk       : All (AttrAllowedIn tag) attrs)
              -> (childPlaceOk  : All (ChildAllowedIn tag) cs)
              -> (childrenValid : All IsValidHtml cs)
              -> IsValidHtml (Element tag attrs cs)

--------------------------------------------------------------------------------
-- Decision procedure (total, structural recursion on HExpr).
--------------------------------------------------------------------------------

decideAttrs : (tag : String) -> (attrs : List HAttr)
           -> Dec (All (AttrAllowedIn tag) attrs)
decideAttrs _ []        = Yes []
decideAttrs t (a :: as) = case decAttrAllowed t a of
  No  contraHd => No (\(hd :: _) => contraHd hd)
  Yes hdOk     => case decideAttrs t as of
    No  contraTl => No (\(_ :: tl) => contraTl tl)
    Yes tlOk     => Yes (hdOk :: tlOk)

decidePlacement : (tag : String) -> (cs : List HExpr)
               -> Dec (All (ChildAllowedIn tag) cs)
decidePlacement _ []        = Yes []
decidePlacement t (c :: cs) = case decChildAllowed t c of
  No  contraHd => No (\(hd :: _) => contraHd hd)
  Yes hdOk     => case decidePlacement t cs of
    No  contraTl => No (\(_ :: tl) => contraTl tl)
    Yes tlOk     => Yes (hdOk :: tlOk)

mutual
  ||| Total decision: returns a proof or a refutation. Never partial.
  public export
  decideHtml : (h : HExpr) -> Dec (IsValidHtml h)
  decideHtml (Text _)               = Yes ValidText
  decideHtml (Comment _)            = Yes ValidComment
  decideHtml (Raw _)                = Yes ValidRaw
  decideHtml (Element t attrs cs)   = case decKnownTag t of
    No  contraT => No (\(ValidElement tg _ _ _) => contraT tg)
    Yes tagOk   => case decideAttrs t attrs of
      No  contraA => No (\(ValidElement _ ats _ _) => contraA ats)
      Yes attrsOk => case decidePlacement t cs of
        No  contraP => No (\(ValidElement _ _ cps _) => contraP cps)
        Yes placeOk => case decideAllValid cs of
          No  contraC => No (\(ValidElement _ _ _ cv) => contraC cv)
          Yes childOk => Yes (ValidElement tagOk attrsOk placeOk childOk)

  ||| Decide `All IsValidHtml cs` by structural recursion.
  public export
  decideAllValid : (cs : List HExpr) -> Dec (All IsValidHtml cs)
  decideAllValid []        = Yes []
  decideAllValid (c :: cs) = case decideHtml c of
    No  contraHd => No (\(hd :: _) => contraHd hd)
    Yes hdOk     => case decideAllValid cs of
      No  contraTl => No (\(_ :: tl) => contraTl tl)
      Yes tlOk     => Yes (hdOk :: tlOk)

||| Kept for backward compatibility with consumers that imported the
||| spike's name. Aliases `decideAllValid`.
public export
decideAll : (cs : List HExpr) -> Dec (All IsValidHtml cs)
decideAll = decideAllValid

--------------------------------------------------------------------------------
-- Located rejection (P2.3).
--------------------------------------------------------------------------------

||| Why a tree fails `IsValidHtml`. Each constructor names the rejection
||| class plain enough for downstream tooling (and humans) to dispatch on.
public export
data RejectionClass : Type where
  ||| Element tag is not in the catalog (`Cribrum.Html.Model.elements`).
  UnknownTag          : (tag : String) -> RejectionClass

  ||| Attribute name is not permitted on this element (not in
  ||| `globalAttrs`, not in `localAttrsOf tag`, not a `data-*` / `aria-*`
  ||| / `on*` attribute).
  DisallowedAttr      : (tag : String) -> (attr : String) -> RejectionClass

  ||| A child element's tag is not permitted directly inside this parent
  ||| under any branch of the catalog policy. The most common form of
  ||| placement failure (e.g. `<p>` containing `<div>`).
  IllegalChild        : (parent : String) -> (childTag : String) -> RejectionClass

  ||| A more specific `IllegalChild` flavour: a flow-only child appears
  ||| inside a phrasing-only parent. Surfaced separately so authors
  ||| recognise the canonical "block in phrasing" mistake.
  BlockInPhrasing     : (parent : String) -> (childTag : String) -> RejectionClass

  ||| A more specific `IllegalChild` flavour: the parent's policy is
  ||| `OnlyTags` (a structural parent) and the offending child does not
  ||| belong to the allowed set. Surfaces `<ul>` containing `<p>`,
  ||| `<table>` containing a stray phrase, etc.
  MalformedTable      : (parent : String) -> (childTag : String) -> RejectionClass

  ||| Bare text appears directly inside a parent that does not accept
  ||| text content (e.g. `<ul>`, `<table>`, `<head>`).
  TextNotAllowedIn    : (parent : String) -> RejectionClass

  ||| A comment appears under a parent that admits no children at all
  ||| (the void-element set: `<br>`, `<img>`, `<hr>`, ...).
  CommentNotAllowedIn : (parent : String) -> RejectionClass

  ||| A comment appears directly inside a raw-text element (`<script>`,
  ||| `<style>`, `<title>`, `<textarea>`). `<!--` in those contexts is
  ||| character data, not an HTML comment, so an `HExpr` `Comment` node
  ||| there cannot be serialised as one. (Spec: raw-text / escapable-
  ||| raw-text content models.)
  CommentInRawText    : (parent : String) -> RejectionClass

  ||| Interactive-content element appears as a descendant of an
  ||| interactive ancestor that forbids them. Concretely:
  |||   * `<a>` content model: "no interactive content descendant and no
  |||     `a` element descendant".
  |||   * `<button>` content model: "no interactive content descendant".
  ||| Path locates the offending descendant; `ancestor` names the
  ||| restricting context and `child` the descendant tag.
  InteractiveInInteractive : (ancestor : String) -> (child : String) -> RejectionClass

  ||| A `<form>` element appears as a descendant of another `<form>`.
  ||| (Spec: form's content model is "flow content, but with no form
  ||| element descendants".)
  FormInForm          : RejectionClass

  ||| An `<li>` element appears outside an `<ol>` / `<ul>` / `<menu>`
  ||| parent. (Spec: li's element-context requirement.)
  OrphanLi            : RejectionClass

  ||| A `<dt>` or `<dd>` element appears outside a `<dl>` (optionally
  ||| wrapped in a `<div>`). `childTag` is `"dt"` or `"dd"`.
  OrphanDtDd          : (childTag : String) -> RejectionClass

public export
Show RejectionClass where
  show (UnknownTag t)               = "UnknownTag " ++ show t
  show (DisallowedAttr t a)         = "DisallowedAttr " ++ show t ++ " " ++ show a
  show (IllegalChild p c)           = "IllegalChild " ++ show p ++ " " ++ show c
  show (BlockInPhrasing p c)        = "BlockInPhrasing " ++ show p ++ " " ++ show c
  show (MalformedTable p c)         = "MalformedTable " ++ show p ++ " " ++ show c
  show (TextNotAllowedIn p)         = "TextNotAllowedIn " ++ show p
  show (CommentNotAllowedIn p)      = "CommentNotAllowedIn " ++ show p
  show (CommentInRawText p)         = "CommentInRawText " ++ show p
  show (InteractiveInInteractive a c) =
    "InteractiveInInteractive " ++ show a ++ " " ++ show c
  show FormInForm                   = "FormInForm"
  show OrphanLi                     = "OrphanLi"
  show (OrphanDtDd t)               = "OrphanDtDd " ++ show t

||| A located rejection: the 0-indexed path into the tree at which the
||| first violation was found, plus the rejection class.
|||
||| `[]` = the root node; `[2, 0]` = "first child of third child of root".
public export
record LocatedReject where
  constructor MkLocatedReject
  path   : List Nat
  reason : RejectionClass

public export
Show LocatedReject where
  show (MkLocatedReject p r) =
    "LocatedReject at " ++ show p ++ ": " ++ show r

--------------------------------------------------------------------------------
-- Located reject helpers.
--------------------------------------------------------------------------------

||| Classify why `child` is not permitted under `parent`. Assumes
||| `childAllowedBool parent child = False`. Pure dispatch on the
||| parent's policy + the child shape; no IO, total.
classifyChildRejection : (parent : String) -> HExpr -> RejectionClass
classifyChildRejection parent (Text _) = case childPolicyOf parent of
  NoChildren            => TextNotAllowedIn parent
  _                     => TextNotAllowedIn parent
-- A `Raw` child is always permitted under a non-void parent
-- (`childAllowedBool _ (Raw _) = True`), so this branch is unreachable;
-- it exists only to keep the match total. `TextNotAllowedIn` is an
-- arbitrary placeholder reason.
classifyChildRejection parent (Raw _) = TextNotAllowedIn parent
classifyChildRejection parent (Comment _) = case childPolicyOf parent of
  NoChildren            => CommentNotAllowedIn parent
  TextOnly              => if isRawTextOf parent
                             then CommentInRawText parent
                             else CommentNotAllowedIn parent
  _                     => CommentNotAllowedIn parent
classifyChildRejection parent (Element ct _ _) = case childPolicyOf parent of
  NoChildren            => IllegalChild parent ct
  TextOnly              => IllegalChild parent ct
  AnyContent            => IllegalChild parent ct      -- unreachable
  OnlyTags _ _          => MalformedTable parent ct
  OnlyCategories cats   =>
    -- If parent allows only Phrasing and the child is Flow but not
    -- Phrasing, surface the canonical "block in phrasing" diagnosis.
    let isPhrasingOnly  = cats == [Phrasing]
        childCats       = categoriesOf ct
        childIsFlowOnly = elem Flow childCats && not (elem Phrasing childCats)
    in if isPhrasingOnly && childIsFlowOnly
         then BlockInPhrasing parent ct
         else IllegalChild parent ct

||| Find the first attribute name on `tag` that fails permission, or
||| `Nothing` if all pass.
firstDisallowedAttr : (tag : String) -> List HAttr -> Maybe String
firstDisallowedAttr _   []                       = Nothing
firstDisallowedAttr tag (MkHAttr a _ :: rest) =
  if isAllowedAttrName tag a
    then firstDisallowedAttr tag rest
    else Just a

||| Walk `cs` looking for the first child whose placement under `parent`
||| fails, returning that child's index + rejection class. An element
||| child whose own tag is not in the catalog surfaces as `UnknownTag`
||| (more useful than the structural placement failure that would
||| otherwise also fire — empty `categoriesOf` makes every unknown
||| element fail every category overlap).
firstPlacementFailure : (parent : String) -> (idx : Nat)
                     -> List HExpr -> Maybe (Nat, RejectionClass)
firstPlacementFailure _      _   []        = Nothing
firstPlacementFailure parent idx (c :: cs) = case c of
  Element t _ _ =>
    if not (isKnownTagBool t)
      then Just (idx, UnknownTag t)
      else if childAllowedBool parent c
        then firstPlacementFailure parent (S idx) cs
        else Just (idx, classifyChildRejection parent c)
  _ =>
    if childAllowedBool parent c
      then firstPlacementFailure parent (S idx) cs
      else Just (idx, classifyChildRejection parent c)

mutual
  ||| Locate the first invalidity in `h`. Returns `Nothing` iff `h`
  ||| validates. Threads a 0-indexed path; root is `[]`.
  locate : (path : List Nat) -> (h : HExpr) -> Maybe LocatedReject
  locate _    (Text _)             = Nothing
  locate _    (Comment _)          = Nothing
  locate _    (Raw _)              = Nothing
  locate path (Element t attrs cs) =
    if not (isKnownTagBool t)
      then Just (MkLocatedReject path (UnknownTag t))
      else case firstDisallowedAttr t attrs of
        Just a  => Just (MkLocatedReject path (DisallowedAttr t a))
        Nothing => case firstPlacementFailure t 0 cs of
          Just (i, r) => Just (MkLocatedReject (path ++ [i]) r)
          Nothing     => locateChildren path 0 cs

  locateChildren : (path : List Nat) -> (idx : Nat) -> List HExpr
                -> Maybe LocatedReject
  locateChildren _    _   []        = Nothing
  locateChildren path idx (c :: cs) = case locate (path ++ [idx]) c of
    Just lr => Just lr
    Nothing => locateChildren path (S idx) cs

--------------------------------------------------------------------------------
-- Ancestor-context checks.
--
-- A second located walk that enforces the rules `IsValidHtml`'s local
-- parent-child predicates cannot express on their own:
--
--   * Interactive-in-interactive — `<a>` and `<button>` forbid
--     interactive-content descendants (and `<a>` additionally forbids
--     `<a>` descendants).
--   * Form-in-form — `<form>` forbids `<form>` descendants.
--   * Orphan `<li>` — `<li>` must live under `<ol>` / `<ul>` / `<menu>`.
--   * Orphan `<dt>` / `<dd>` — must live under `<dl>` (optionally wrapped
--     in `<div>`).
--
-- The witness type `IsValidHtml h` does NOT capture these (each rule
-- depends on ancestor state, not the local subtree). The ancestor pass
-- therefore runs as a side check inside `decideHtmlLocated`: a tree
-- that fails ancestor context is rejected with the appropriate
-- `RejectionClass`, even when `decideHtml h = Yes`. The structural
-- `IsValidHtml h` witness for such trees is correct as a structural
-- proof but never reaches the caller (oracle / elaborator), preserving
-- the contract that "decided" reports the full HTML5 verdict.
--
-- This mirrors the layering used for `StructuralAA`: a sibling
-- proposition / decision pass conjoined at the boundary, not folded
-- into the core IR proof.
--------------------------------------------------------------------------------

||| Ancestor state accumulated while walking the tree. Tracks the
||| subset of ancestor element kinds that gate descendant placement.
record AncestorCtx where
  constructor MkAncestorCtx
  ||| Inside an `<a>` ancestor — forbids interactive descendants.
  inAnchor     : Bool
  ||| Inside a `<button>` ancestor — forbids interactive descendants.
  inButton     : Bool
  ||| Inside a `<form>` ancestor — forbids `<form>` descendants.
  inForm       : Bool
  ||| Inside a `<ul>` / `<ol>` / `<menu>` ancestor — required for `<li>`.
  inListParent : Bool
  ||| Inside a `<dl>` ancestor — required for `<dt>` / `<dd>`.
  ||| (`<div>` wrappers under `<dl>` keep this flag true so a `<dt>`
  ||| nested through a div still counts; modelled here by leaving
  ||| `inDl` untouched on `<div>` entry.)
  inDl         : Bool

rootCtx : AncestorCtx
rootCtx = MkAncestorCtx False False False False False

isListParent : String -> Bool
isListParent t = t == "ul" || t == "ol" || t == "menu"

isInteractiveTag : String -> Bool
isInteractiveTag t = elem Interactive (categoriesOf t)

||| Update the ancestor context when entering element `tag`. Only the
||| flagged ancestor kinds toggle; everything else passes through.
enterCtx : AncestorCtx -> (tag : String) -> AncestorCtx
enterCtx ctx tag =
  let ctx1 = if tag == "a"          then { inAnchor     := True } ctx  else ctx
      ctx2 = if tag == "button"     then { inButton     := True } ctx1 else ctx1
      ctx3 = if tag == "form"       then { inForm       := True } ctx2 else ctx2
      ctx4 = if isListParent tag    then { inListParent := True } ctx3 else ctx3
      ctx5 = if tag == "dl"         then { inDl         := True } ctx4 else ctx4
   in ctx5

||| Classify the ancestor-context violation for visiting element `tag`
||| under context `ctx`, or `Nothing` if there is no violation.
selfAncestorReject : AncestorCtx -> (tag : String) -> Maybe RejectionClass
selfAncestorReject ctx tag =
  if tag == "form" && inForm ctx then Just FormInForm
  else if tag == "li" && not (inListParent ctx) then Just OrphanLi
  else if (tag == "dt" || tag == "dd") && not (inDl ctx) then Just (OrphanDtDd tag)
  else if (inAnchor ctx || inButton ctx) && isInteractiveTag tag then
    Just (InteractiveInInteractive
            (if inAnchor ctx then "a" else "button")
            tag)
  else Nothing

mutual
  locateAncestor : AncestorCtx -> List Nat -> HExpr -> Maybe LocatedReject
  locateAncestor _   _    (Text _)           = Nothing
  locateAncestor _   _    (Comment _)        = Nothing
  locateAncestor _   _    (Raw _)            = Nothing
  locateAncestor ctx path (Element t _ cs) = case selfAncestorReject ctx t of
    Just rc => Just (MkLocatedReject path rc)
    Nothing =>
      let ctx' = enterCtx ctx t
       in locateAncestorList ctx' path 0 cs

  locateAncestorList : AncestorCtx -> List Nat -> Nat -> List HExpr
                    -> Maybe LocatedReject
  locateAncestorList _   _    _   []        = Nothing
  locateAncestorList ctx path idx (c :: cs) =
    case locateAncestor ctx (path ++ [idx]) c of
      Just lr => Just lr
      Nothing => locateAncestorList ctx path (S idx) cs

||| Decide validity *and* produce a located rejection when invalid.
||| If `decideHtml` returns `Yes p`, this returns `Right p` *only when*
||| the ancestor-context pass also passes. Ancestor failure produces a
||| `Left` even though the structural proof exists — see the section
||| comment above.
public export
decideHtmlLocated : (h : HExpr) -> Either LocatedReject (IsValidHtml h)
decideHtmlLocated h = case decideHtml h of
  Yes p => case locateAncestor rootCtx [] h of
    Just lr => Left lr
    Nothing => Right p
  No  _ => case locate [] h of
    Just lr => Left lr
    Nothing => Left (MkLocatedReject [] (UnknownTag "<internal: located/dec disagree>"))

--------------------------------------------------------------------------------
-- Convenience.
--------------------------------------------------------------------------------

||| `True` iff `h` passes the *local* structural content-model check
||| (`decideHtml`). Does NOT include the ancestor-context pass — a tree
||| like `<a><button>` will report `True` here even though
||| `decideHtmlLocated` rejects it. Use `isValidHtmlLocated` (or
||| `decideHtmlLocated` directly) when the full HTML5 verdict is
||| required.
public export
isValidHtml : HExpr -> Bool
isValidHtml h = case decideHtml h of
  Yes _ => True
  No  _ => False

||| `True` iff `h` passes both the structural content-model check and
||| the ancestor-context pass. This is the verdict the oracle and the
||| strict elaborator consume; callers that need to *carry* a proof
||| should still go through `decideHtmlLocated` (the structural witness
||| comes with the `Right`).
public export
isValidHtmlLocated : HExpr -> Bool
isValidHtmlLocated h = case decideHtmlLocated h of
  Right _ => True
  Left  _ => False
