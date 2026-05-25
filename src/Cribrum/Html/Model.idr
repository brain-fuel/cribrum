||| HTML element catalog — Phase 2 per `plan.dj` §Phase 2 (P2.1).
|||
||| Public surface for the HTML model: types
||| (`Cribrum.Html.Model.Types`) + spec data
||| (`Cribrum.Html.Model.Generated`) + lookup behavior + attribute-name
||| permission. The catalog is **ingested** from `ingest/content-model.ts`
||| via `ingest/html-model.ts`, with `@webref/elements` providing
||| cross-validation; this module never hand-lists element rows.
|||
||| Drift gate: `make ingest-check` re-runs the ingestion and diffs
||| `Cribrum.Html.Model.Generated`'s output. Any divergence (upstream
||| `@webref` bump, `ingest/content-model.ts` edit, hand-edit of
||| `Generated.idr`) fails the gate; rerun `make ingest` to refresh.
module Cribrum.Html.Model

import Data.List
import Cribrum.Html.Category
import public Cribrum.Html.Model.Types
import public Cribrum.Html.Model.Generated

%default total

--------------------------------------------------------------------------------
-- Global attributes (apply to every element). Sourced from the generated
-- catalog; this binding gives downstream code a stable name.
--------------------------------------------------------------------------------

||| HTML global attributes per WHATWG §3.2.6. Element-specific catalogues
||| (`localAttrs`) extend this set.
|||
||| Includes the `aria-*` and `data-*` namespaces by literal prefix; the
||| `isAllowedAttrName` decision below handles the prefix logic.
public export
globalAttrs : List String
globalAttrs = generatedGlobalAttrs

--------------------------------------------------------------------------------
-- Element catalog. Order is irrelevant to the validator; the ingestion
-- script emits rows sorted lexicographically by `name`.
--------------------------------------------------------------------------------

||| The catalog.
public export
elements : List ElementSpec
elements = generatedElements

--------------------------------------------------------------------------------
-- Lookups.
--------------------------------------------------------------------------------

||| Find the spec for `tag` in the catalog. Returns `Nothing` if the tag
||| is not in the catalog (unknown elements are rejected upstream by
||| `IsKnownTag` in `Cribrum.Html.Valid`; this lookup is the catalog-side
||| half of that decision).
public export
lookupSpec : String -> Maybe ElementSpec
lookupSpec t = find (\s => name s == t) elements

||| `True` iff `tag` is in the catalog.
public export
isKnownTagBool : String -> Bool
isKnownTagBool t = case lookupSpec t of
  Just _  => True
  Nothing => False

||| Convenience: the catalog's allowed tag names.
public export
knownTagNames : List String
knownTagNames = map name elements

||| Categories assigned to `tag`. Unknown tag → empty list.
public export
categoriesOf : String -> List Category
categoriesOf t = case lookupSpec t of
  Just s  => categories s
  Nothing => []

||| Permitted-content policy for `tag`. Unknown tag → `NoChildren` (the
||| safest fallback; the unknown-tag rejection fires first).
public export
childPolicyOf : String -> ChildPolicy
childPolicyOf t = case lookupSpec t of
  Just s  => childPolicy s
  Nothing => NoChildren

||| Local attributes for `tag` (does NOT include `globalAttrs`). Unknown
||| tag → empty list.
public export
localAttrsOf : String -> List String
localAttrsOf t = case lookupSpec t of
  Just s  => localAttrs s
  Nothing => []

||| `True` iff `tag` is in HTML 5's void-element set per the catalog.
public export
isVoidTag : String -> Bool
isVoidTag t = case lookupSpec t of
  Just s  => isVoid s
  Nothing => False

--------------------------------------------------------------------------------
-- Attribute name permission.
--
-- An attribute name is allowed on a given element iff it is:
--   1. In `globalAttrs`, OR
--   2. In the element's `localAttrs`, OR
--   3. A `data-*` attribute (custom data), OR
--   4. An `aria-*` attribute (ARIA), OR
--   5. An `on*` event handler attribute (HTML allows these on most elements).
--
-- The decision is total + boolean; the type-level lift lives in
-- `Cribrum.Html.Valid`.
--------------------------------------------------------------------------------

isDataAttr : String -> Bool
isDataAttr s = case unpack s of
  ('d' :: 'a' :: 't' :: 'a' :: '-' :: _) => True
  _                                       => False

isAriaAttr : String -> Bool
isAriaAttr s = case unpack s of
  ('a' :: 'r' :: 'i' :: 'a' :: '-' :: _) => True
  _                                       => False

isOnEventAttr : String -> Bool
isOnEventAttr s = case unpack s of
  ('o' :: 'n' :: _) => True
  _                 => False

||| Decide whether `attrName` is permitted on element `tag`.
||| Total. Used by `Cribrum.Html.Valid` to build the `AttrAllowedIn`
||| witness.
public export
isAllowedAttrName : (tag : String) -> (attrName : String) -> Bool
isAllowedAttrName tag a =
     elem a globalAttrs
  || elem a (localAttrsOf tag)
  || isDataAttr a
  || isAriaAttr a
  || isOnEventAttr a
