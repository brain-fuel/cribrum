||| Type-level cross-check of the generated HTML catalog.
|||
||| The ingestion script (`ingest/html-model.ts`) enforces tag-closure
||| (invariant I2) in TypeScript: every `OnlyTags` reference must
||| correspond to an element row. This module lifts that invariant to a
||| decidable Idris proposition over `Cribrum.Html.Model.elements`. The
||| test suite asserts the catalog satisfies it at module-load time, so
||| ingestion's internal consistency is a *checked* proof, not a
||| TypeScript assertion alone.
module Cribrum.Html.Model.Invariants

import Data.List
import Data.List.Elem
import Data.List.Quantifiers
import Decidable.Equality

import Cribrum.Html.Model
import Cribrum.Html.Model.Types

%default total

||| The closure obligation for one `ChildPolicy`. Only `OnlyTags`
||| carries a non-trivial obligation; every other policy is vacuous.
public export
PolicyClosed : (names : List String) -> ChildPolicy -> Type
PolicyClosed names (OnlyTags ts _) = All (\t => Elem t names) ts
PolicyClosed _     _               = ()

||| Decide the per-policy obligation. Total.
public export
decPolicyClosed : (names : List String)
               -> (p : ChildPolicy)
               -> Dec (PolicyClosed names p)
decPolicyClosed names (OnlyTags ts _)    = All.all (\t => isElem t names) ts
decPolicyClosed _     NoChildren         = Yes ()
decPolicyClosed _     (OnlyCategories _) = Yes ()
decPolicyClosed _     TextOnly           = Yes ()
decPolicyClosed _     AnyContent         = Yes ()

||| The closure obligation for one row of the catalog: if the row's
||| content policy lists child tag names (`OnlyTags`), every such name
||| must itself name a catalog row.
public export
ChildClosed : (names : List String) -> ElementSpec -> Type
ChildClosed names s = PolicyClosed names (childPolicy s)

||| Tag closure over the whole catalog (invariant I2 from the
||| ingestion's invariant table).
public export
AllChildTagsExist : List ElementSpec -> Type
AllChildTagsExist es = All (ChildClosed (map name es)) es

||| Decide the per-row obligation. Total.
public export
decChildClosed : (names : List String)
              -> (s : ElementSpec)
              -> Dec (ChildClosed names s)
decChildClosed names s = decPolicyClosed names (childPolicy s)

||| Decide the whole-catalog obligation. Total.
public export
decAllChildTagsExist : (es : List ElementSpec) -> Dec (AllChildTagsExist es)
decAllChildTagsExist es = All.all (decChildClosed (map name es)) es
