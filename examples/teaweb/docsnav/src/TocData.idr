||| Shared record for a table-of-contents entry. Lives in its own module
||| so the auto-generated `Generated.idr` (TOC items harvested from
||| `README.dj`) can import it without pulling in `Main`'s view code.
|||
||| `level` mirrors the heading hierarchy: `1` for top-level (`<h2>`
||| sections), `2` for nested (`<h3>` subsections). Anything deeper
||| collapses to `2` at harvest time.
module TocData

%default total

public export
record TocItem where
  constructor MkTocItem
  anchor : String
  title  : String
  level  : Nat
