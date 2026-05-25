||| HTML element model — types only.
|||
||| Types factored out of `Cribrum.Html.Model` so the generated catalog
||| (`Cribrum.Html.Model.Generated`) can reference them without
||| reintroducing an import cycle. `Cribrum.Html.Model` re-exports this
||| module's contents so direct consumers can keep importing
||| `Cribrum.Html.Model`.
module Cribrum.Html.Model.Types

import Cribrum.Html.Category

%default total

--------------------------------------------------------------------------------
-- ChildPolicy: the permitted-content shape for each element.
--------------------------------------------------------------------------------

||| The content model of a single element, in a form the validator can
||| interpret directly. Five disjoint cases cover every element in the
||| current catalog.
public export
data ChildPolicy : Type where
  ||| Void element: no children of any kind. The HTML void-element set
  ||| (`<br>`, `<img>`, `<input>`, etc.) plus anything else with no
  ||| permitted content.
  NoChildren     : ChildPolicy

  ||| Only the listed child element tags are allowed. `allowText`
  ||| controls whether bare `Text` nodes are also accepted (almost
  ||| always `False` for structural parents like `<ul>` / `<table>`).
  ||| `Comment` nodes are always allowed under this policy.
  OnlyTags       : (tags : List String) -> (allowText : Bool) -> ChildPolicy

  ||| Children must belong to at least one of the listed content
  ||| categories. `Text` is treated as `Phrasing` (the spec's "phrasing
  ||| content" includes character data). `Comment` is always allowed.
  OnlyCategories : (cats : List Category) -> ChildPolicy

  ||| Only text (and comment) nodes — raw-text / escapable-raw-text
  ||| elements (`<script>`, `<style>`, `<title>`, `<textarea>`).
  TextOnly       : ChildPolicy

  ||| Transparent / unspecified — any well-formed child is accepted.
  ||| Used both for genuinely transparent elements (`<a>`, `<ins>`,
  ||| `<del>`, `<map>`, `<slot>`) and as a temporary escape hatch
  ||| for elements whose content model has not yet been refined.
  AnyContent     : ChildPolicy

public export
Show ChildPolicy where
  show NoChildren                 = "NoChildren"
  show (OnlyTags ts at)           =
    "OnlyTags " ++ show ts ++ " (allowText=" ++ show at ++ ")"
  show (OnlyCategories cs)        = "OnlyCategories " ++ show cs
  show TextOnly                   = "TextOnly"
  show AnyContent                 = "AnyContent"

--------------------------------------------------------------------------------
-- ElementSpec: one row of the catalog.
--------------------------------------------------------------------------------

public export
record ElementSpec where
  constructor MkElementSpec
  name         : String
  isVoid       : Bool
  isRawText    : Bool
  categories   : List Category
  childPolicy  : ChildPolicy
  localAttrs   : List String

public export
Show ElementSpec where
  show (MkElementSpec n v r cs cp la) =
    "MkElementSpec " ++ show n
      ++ " void=" ++ show v
      ++ " raw="  ++ show r
      ++ " cats=" ++ show cs
      ++ " cp="   ++ show cp
      ++ " attrs="++ show la
