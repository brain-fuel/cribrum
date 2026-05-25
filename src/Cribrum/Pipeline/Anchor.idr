||| Heading-anchor pipeline pass — plan §T6 demo deliverable plumbing.
|||
||| `addHeadingIds` walks an `HExpr` and attaches an `id="<slug>"`
||| attribute to every `<h1>`..`<h6>` that lacks one, deriving the slug
||| from the heading's plain-text children via `slugify`. Slugs are
||| deduplicated against earlier headings in the document by appending
||| `-2`, `-3`, ... so the produced tree never violates the
||| `duplicate-id` AA rule.
|||
||| `harvestHeadings` walks the *anchored* tree and emits one
||| `(level, anchor, plainTitle)` row per heading in document order.
||| The TEAWeb T6 docs-nav island consumes this to populate its
||| table-of-contents pane — anchors match the heading ids exactly, so
||| in-page navigation works without manual sync.
|||
||| Both passes are total. `addHeadingIds . addHeadingIds = addHeadingIds`
||| (idempotent — the second pass sees ids on every heading and leaves
||| them alone).
module Cribrum.Pipeline.Anchor

import Data.List
import Data.String
import Cribrum.Node

%default total

--------------------------------------------------------------------------------
-- ASCII utilities (no Unicode case folding — matches the T6 search rule).
--------------------------------------------------------------------------------

isAsciiAlpha : Char -> Bool
isAsciiAlpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAsciiDigit : Char -> Bool
isAsciiDigit c = c >= '0' && c <= '9'

isSlugChar : Char -> Bool
isSlugChar c = isAsciiAlpha c || isAsciiDigit c

toLowerAscii : Char -> Char
toLowerAscii c =
  if c >= 'A' && c <= 'Z' then chr (ord c + 32) else c

--------------------------------------------------------------------------------
-- Slugify.
--------------------------------------------------------------------------------

||| Map a character to either its lowercase form (if alphanumeric) or
||| a hyphen separator.
foldChar : Char -> Char
foldChar c =
  let lc = toLowerAscii c
   in if isSlugChar lc then lc else '-'

||| Collapse consecutive hyphens into one. Structural recursion over the
||| input list keeps Idris happy: we only ever recurse on the second-cons.
collapseHyphens : List Char -> List Char
collapseHyphens []                = []
collapseHyphens [c]               = [c]
collapseHyphens (c1 :: c2 :: rest) =
  if c1 == '-' && c2 == '-'
    then collapseHyphens (c2 :: rest)
    else c1 :: collapseHyphens (c2 :: rest)

dropLeadingHyphens : List Char -> List Char
dropLeadingHyphens ('-' :: rest) = dropLeadingHyphens rest
dropLeadingHyphens xs            = xs

trimHyphens : List Char -> List Char
trimHyphens xs = reverse (dropLeadingHyphens (reverse (dropLeadingHyphens xs)))

||| Slugify: ASCII-fold, replace runs of non-alphanumerics with single
||| hyphens, trim leading/trailing hyphens. Empty input -> empty output.
public export
slugify : String -> String
slugify s = pack (trimHyphens (collapseHyphens (map foldChar (unpack s))))

--------------------------------------------------------------------------------
-- Plain-text projection (used for slug source + TOC title).
--------------------------------------------------------------------------------

||| Concatenate every text node's content in pre-order. Comments contribute
||| nothing; element wrappers are transparent.
public export
plainText : HExpr -> String
plainText (Text s)         = s
plainText (Comment _)      = ""
plainText (Element _ _ cs) =
  concatMap (assert_total plainText) cs

childrenText : List HExpr -> String
childrenText cs = concatMap plainText cs

--------------------------------------------------------------------------------
-- Heading classification.
--------------------------------------------------------------------------------

||| Heading level if the tag is `h1`..`h6`; `Nothing` otherwise.
public export
headingLevel : String -> Maybe Nat
headingLevel "h1" = Just 1
headingLevel "h2" = Just 2
headingLevel "h3" = Just 3
headingLevel "h4" = Just 4
headingLevel "h5" = Just 5
headingLevel "h6" = Just 6
headingLevel _    = Nothing

isHeading : String -> Bool
isHeading t = case headingLevel t of
  Just _  => True
  Nothing => False

--------------------------------------------------------------------------------
-- Attribute helpers (kept local; not worth a third copy of `hasAttr` in
-- the public surface).
--------------------------------------------------------------------------------

hasAttrL : String -> List HAttr -> Bool
hasAttrL name = any (\a => name == a.name)

attrValueL : String -> List HAttr -> Maybe String
attrValueL _    []                            = Nothing
attrValueL name (MkHAttr n (Str s) :: rest)   =
  if n == name then Just s else attrValueL name rest
attrValueL name (_ :: rest)                   = attrValueL name rest

--------------------------------------------------------------------------------
-- Disambiguation: pick `base`, `base-2`, `base-3`, ... not yet in `seen`.
--------------------------------------------------------------------------------

candidate : String -> Nat -> String
candidate base 1 = base
candidate base n = base ++ "-" ++ show n

||| Choose the lowest `n >= 1` such that `candidate base n` is not in `seen`.
||| Bounded by `length seen + 1` (pigeon-hole: only that many distinct
||| candidates can clash).
disambiguate : String -> List String -> String
disambiguate base seen = pickFrom 1 (S (length seen))
  where
    pickFrom : (n : Nat) -> (fuel : Nat) -> String
    pickFrom n Z        = candidate base n     -- fuel exhausted — must be free
    pickFrom n (S k)    =
      let c = candidate base n
       in if elem c seen
            then pickFrom (S n) k
            else c

--------------------------------------------------------------------------------
-- addHeadingIds: pre-order traversal threading the seen-slug list.
--------------------------------------------------------------------------------

||| Walk one node. Returns the rewritten node and the updated seen-slug
||| list (parent assigns its slug *before* its children's slugs are
||| computed, so descendants see the ancestor's slug when disambiguating).
walkNode : List String -> HExpr -> (List String, HExpr)

walkNodeList : List String -> List HExpr -> (List String, List HExpr)
walkNodeList seen []        = (seen, [])
walkNodeList seen (h :: hs) =
  let (seen1, h')   = walkNode seen h
      (seen2, hs') = walkNodeList seen1 hs
   in (seen2, h' :: hs')

walkNode seen (Text s)    = (seen, Text s)
walkNode seen (Comment s) = (seen, Comment s)
walkNode seen (Element t attrs cs) =
  let (seen1, attrs') =
        if isHeading t && not (hasAttrL "id" attrs)
          then
            let raw = slugify (childrenText cs)
                base = if raw == "" then "section" else raw
                slug = disambiguate base seen
             in (slug :: seen, attrs ++ [MkHAttr "id" (Str slug)])
          else
            -- Pre-existing id (or non-heading): just record any present id
            -- so descendant headings don't accidentally collide with it.
            case attrValueL "id" attrs of
              Just v  => (v :: seen, attrs)
              Nothing => (seen, attrs)
      (seen2, cs') = walkNodeList seen1 cs
   in (seen2, Element t attrs' cs')

||| Decorate every unanchored heading with an `id="<slug>"` attribute.
||| Idempotent — re-running on an already-anchored tree is a no-op.
||| Existing ids are honoured (and never overwritten); any heading whose
||| plain text slugifies to the empty string falls back to `section`.
public export
addHeadingIds : HExpr -> HExpr
addHeadingIds h = snd (walkNode [] h)

--------------------------------------------------------------------------------
-- harvestHeadings: pre-order list of (level, anchor, plainTitle).
--------------------------------------------------------------------------------

||| Walk the tree in document order; emit one row per heading. The anchor
||| is taken from the heading's `id` attribute if present, else "" — call
||| `addHeadingIds` first if you want the slugs filled in.
public export
harvestHeadings : HExpr -> List (Nat, String, String)
harvestHeadings (Text _)        = []
harvestHeadings (Comment _)     = []
harvestHeadings (Element t attrs cs) = case headingLevel t of
  Just lvl =>
    let anchor = case attrValueL "id" attrs of
                   Just v  => v
                   Nothing => ""
        title  = childrenText cs
        here   = (lvl, anchor, title)
     in here :: concatMap (assert_total harvestHeadings) cs
  Nothing =>
    concatMap (assert_total harvestHeadings) cs
