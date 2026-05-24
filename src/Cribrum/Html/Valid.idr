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
||| * `TextOnly`          — only `Text` / `Comment`.
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
    Comment _             => True
    Element _ _ _         => False
  AnyContent            => True
  OnlyTags ts allowText => case c of
    Text _                => allowText
    Comment _             => True
    Element t _ _         => elem t ts
  OnlyCategories cats   => case c of
    -- Text is phrasing content; phrasing ⊆ flow, so a parent that
    -- accepts Flow also accepts text. This subsumption mirrors the
    -- spec: any phrasing-content node is also flow content.
    Text _                => elem Phrasing cats || elem Flow cats
    Comment _             => True
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

public export
Show RejectionClass where
  show (UnknownTag t)             = "UnknownTag " ++ show t
  show (DisallowedAttr t a)       = "DisallowedAttr " ++ show t ++ " " ++ show a
  show (IllegalChild p c)         = "IllegalChild " ++ show p ++ " " ++ show c
  show (BlockInPhrasing p c)      = "BlockInPhrasing " ++ show p ++ " " ++ show c
  show (MalformedTable p c)       = "MalformedTable " ++ show p ++ " " ++ show c
  show (TextNotAllowedIn p)       = "TextNotAllowedIn " ++ show p
  show (CommentNotAllowedIn p)    = "CommentNotAllowedIn " ++ show p

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
classifyChildRejection parent (Comment _) = case childPolicyOf parent of
  NoChildren            => CommentNotAllowedIn parent
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

||| Decide validity *and* produce a located rejection when invalid.
||| If `decideHtml` returns `Yes p`, this returns `Right p`. Otherwise
||| `locate` runs and surfaces the first violation; we use a synthetic
||| fallback `LocatedReject` if the locator and the decider disagree
||| (impossible by construction — both interpret the same predicates).
public export
decideHtmlLocated : (h : HExpr) -> Either LocatedReject (IsValidHtml h)
decideHtmlLocated h = case decideHtml h of
  Yes p => Right p
  No  _ => case locate [] h of
    Just lr => Left lr
    Nothing => Left (MkLocatedReject [] (UnknownTag "<internal: located/dec disagree>"))

--------------------------------------------------------------------------------
-- Convenience.
--------------------------------------------------------------------------------

||| `True` iff `h` is structurally valid HTML. Use `decideHtml` directly
||| when you need to *carry* the proof (the whole point of the indexed
||| proposition).
public export
isValidHtml : HExpr -> Bool
isValidHtml h = case decideHtml h of
  Yes _ => True
  No  _ => False
