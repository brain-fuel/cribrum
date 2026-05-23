||| `IsValidHtml` — Phase 2 spike per `plan.md` §Phase 2.
|||
||| HTML well-formedness as an indexed proposition `IsValidHtml : HExpr -> Type`
||| with a total decision procedure `decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`.
||| Conformance is a *proof*, never a separate datatype: this is the single-type
||| invariant in action.
|||
||| Scope of this spike (deliberately minimal — full content model lands in a
||| later iteration with the ingestion pipeline):
|||
|||   - `IsKnownTag tag` : the tag is in a hard-coded subset of HTML elements.
|||   - `IsValidHtml h`  : if `h` is `Element t _ cs` then `IsKnownTag t` AND
|||     every child is `IsValidHtml`; text/comment nodes are trivially valid.
|||
||| Attribute permission, per-element content-model constraints, and the full
||| element catalog are explicitly OUT of scope here; this module exists to
||| prove the *machinery* (indexed prop + Dec) works on HExpr and to lock the
||| API that downstream consumers (elaboration's codomain in Phase 1b, AA
||| types in Phase 4) will demand.
module Cribrum.Html.Valid

import Data.List
import Data.List.Elem
import Data.List.Quantifiers
import Decidable.Equality
import Cribrum.Node

%default total

--------------------------------------------------------------------------------
-- Known-tag set (spike subset; full catalog comes from ingestion).
--------------------------------------------------------------------------------

||| The seed subset. Future iteration: read this from the HTML spec's
||| machine-readable element definitions (per plan.md §Phase 2 "ingested
||| from machine-readable upstreams").
public export
knownTags : List String
knownTags =
  [ "html", "head", "body", "title", "meta", "link"
  , "p", "div", "span", "br", "hr"
  , "h1", "h2", "h3", "h4", "h5", "h6"
  , "ul", "ol", "li"
  , "a", "img", "figure", "figcaption"
  , "section", "article", "nav", "aside", "main", "header", "footer"
  , "pre", "code", "blockquote"
  ]

||| Decidable membership in `knownTags`.
public export
IsKnownTag : String -> Type
IsKnownTag t = Elem t knownTags

public export
decKnownTag : (t : String) -> Dec (IsKnownTag t)
decKnownTag t = isElem t knownTags

--------------------------------------------------------------------------------
-- IsValidHtml.
--------------------------------------------------------------------------------

||| Indexed proposition: structural well-formedness over HExpr.
|||
||| `ValidText` / `ValidComment`: leaves are trivially valid.
||| `ValidElement`: tag is known AND every child is itself valid.
public export
data IsValidHtml : HExpr -> Type where
  ValidText    : IsValidHtml (Text s)
  ValidComment : IsValidHtml (Comment s)
  ValidElement : (tagOk    : IsKnownTag tag)
              -> (childrenOk : All IsValidHtml cs)
              -> IsValidHtml (Element tag attrs cs)

--------------------------------------------------------------------------------
-- Decision procedure (total, structural recursion on HExpr).
--------------------------------------------------------------------------------

mutual
  ||| Total decision: returns a proof or a refutation. Never partial.
  public export
  decideHtml : (h : HExpr) -> Dec (IsValidHtml h)
  decideHtml (Text _)            = Yes ValidText
  decideHtml (Comment _)         = Yes ValidComment
  decideHtml (Element t a cs) = case decKnownTag t of
    No  contraT => No (\(ValidElement tagOk _) => contraT tagOk)
    Yes tagOk   => case decideAll cs of
      No  contraC => No (\(ValidElement _ childOk) => contraC childOk)
      Yes childOk => Yes (ValidElement tagOk childOk)

  ||| Decide `All IsValidHtml cs` by structural recursion.
  public export
  decideAll : (cs : List HExpr) -> Dec (All IsValidHtml cs)
  decideAll []        = Yes []
  decideAll (c :: cs) = case decideHtml c of
    No  contraHead => No (\(headOk :: _) => contraHead headOk)
    Yes headOk     => case decideAll cs of
      No  contraTail => No (\(_ :: tailOk) => contraTail tailOk)
      Yes tailOk     => Yes (headOk :: tailOk)

--------------------------------------------------------------------------------
-- Convenience.
--------------------------------------------------------------------------------

||| `True` iff `h` is structurally valid HTML. Use `decideHtml` directly when
||| you need to *carry* the proof (the whole point of the indexed proposition).
public export
isValidHtml : HExpr -> Bool
isValidHtml h = case decideHtml h of
  Yes _ => True
  No  _ => False
