||| Section-anchor pipeline pass — plan §1b heading-inference plumbing +
||| §T6 demo deliverable.
|||
||| `elaborate` wraps heading-level sequences in nested `<section>`
||| landmarks but leaves auto-`id`s unfilled (so the strict codomain's
||| `duplicate-id` check never sees a disambiguator-generated id).
||| `addSectionIds` is the post-pass that fills them in: it walks an
||| `HExpr` and attaches an `id="<autoId>"` attribute to every
||| `<section>` that lacks one, deriving the value from the section's
||| heading text via `autoId`. Ids are deduplicated against every id
||| already present in the document (explicit `{#id}` on any element,
||| plus earlier auto-ids) by appending `-1`, `-2`, ... — the Djot
||| reference identifier scheme — so the produced tree never violates the
||| `duplicate-id` AA rule.
|||
||| `harvestHeadings` walks the *anchored* tree and emits one
||| `(level, anchor, plainTitle)` row per section in document order, where
||| `anchor` is the section id and `level`/`plainTitle` come from the
||| section's heading. The TEAWeb T6 docs-nav island consumes this to
||| populate its table-of-contents pane — anchors match the section ids
||| exactly, so in-page navigation works without manual sync.
|||
||| `autoId` matches the Djot reference auto-identifier: runs of non
||| `[A-Za-z0-9_]` characters collapse to a single `-`, leading/trailing
||| `-` are trimmed, and case is *preserved* (Djot does not lowercase).
|||
||| Both passes are total. `addSectionIds . addSectionIds = addSectionIds`
||| (idempotent — the second pass sees ids on every section and leaves
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

||| A character that survives into an auto-identifier verbatim: ASCII
||| alphanumerics plus underscore (matching Djot's `\w`). Everything else
||| is a separator.
isIdentChar : Char -> Bool
isIdentChar c = isAsciiAlpha c || isAsciiDigit c || c == '_'

--------------------------------------------------------------------------------
-- autoId — Djot reference auto-identifier.
--------------------------------------------------------------------------------

||| Map a character to itself (if an identifier char) or a hyphen
||| separator. Case is preserved — Djot's identifier algorithm does not
||| lowercase.
foldChar : Char -> Char
foldChar c = if isIdentChar c then c else '-'

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

||| Djot reference auto-identifier: replace runs of non-`[A-Za-z0-9_]`
||| with single hyphens, trim leading/trailing hyphens, preserve case.
||| Empty / all-separator input -> empty output (the caller supplies a
||| fallback base for empty headings).
public export
autoId : String -> String
autoId s = pack (trimHyphens (collapseHyphens (map foldChar (unpack s))))

--------------------------------------------------------------------------------
-- Plain-text projection (used for id source + TOC title).
--------------------------------------------------------------------------------

||| Concatenate every text node's content in pre-order. Comments contribute
||| nothing; element wrappers are transparent.
public export
plainText : HExpr -> String
plainText (Text s)         = s
plainText (Comment _)      = ""
plainText (Raw _)          = ""
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
-- Section heading text: the plain text of a section's direct heading
-- child (the first `<h1>`..`<h6>` among its children — nested sections'
-- headings are NOT considered).
--------------------------------------------------------------------------------

sectionHeadingText : List HExpr -> String
sectionHeadingText []                       = ""
sectionHeadingText (Element t _ cs :: rest) =
  if isHeading t then childrenText cs else sectionHeadingText rest
sectionHeadingText (_ :: rest)              = sectionHeadingText rest

--------------------------------------------------------------------------------
-- Disambiguation: pick `base`, `base-1`, `base-2`, ... not yet in `seen`
-- (the Djot reference identifier scheme: the un-suffixed form first, then
-- `-1`-based suffixes).
--------------------------------------------------------------------------------

candidate : String -> Nat -> String
candidate base 0 = base
candidate base n = base ++ "-" ++ show n

||| Choose the lowest `n >= 0` such that `candidate base n` is not in
||| `seen`. Bounded by `length seen + 1` (pigeon-hole: only that many
||| distinct candidates can clash).
||| Begin numbering at `start`. With `start = 1` the un-suffixed `base`
||| is never produced, so an empty heading falls back to `s-1`, `s-2`, …
||| (the Djot reference scheme for id-less sections derived from empty
||| text). `start = 0` is the ordinary "un-suffixed form first" path.
disambiguateFrom : (start : Nat) -> String -> List String -> String
disambiguateFrom start base seen = pickFrom start (S (start + length seen))
  where
    pickFrom : (n : Nat) -> (fuel : Nat) -> String
    pickFrom n Z     = candidate base n     -- fuel exhausted — must be free
    pickFrom n (S k) =
      let c = candidate base n
       in if elem c seen
            then pickFrom (S n) k
            else c

disambiguate : String -> List String -> String
disambiguate base seen = disambiguateFrom 0 base seen

--------------------------------------------------------------------------------
-- addSectionIds: pre-order traversal threading the seen-id list.
--------------------------------------------------------------------------------

||| Walk one node. Returns the rewritten node and the updated seen-id
||| list. A `<section>` assigns its id *before* its children are walked,
||| so a nested section disambiguates against its ancestor's id.
walkNode : List String -> HExpr -> (List String, HExpr)

walkSectionChildren : (skipHeading : Bool) -> List String -> List HExpr
                   -> (List String, List HExpr)

walkNodeList : List String -> List HExpr -> (List String, List HExpr)
walkNodeList seen []        = (seen, [])
walkNodeList seen (h :: hs) =
  let (seen1, h')  = walkNode seen h
      (seen2, hs') = walkNodeList seen1 hs
   in (seen2, h' :: hs')

walkNode seen (Text s)    = (seen, Text s)
walkNode seen (Comment s) = (seen, Comment s)
walkNode seen (Raw s)     = (seen, Raw s)
walkNode seen (Element t attrs cs) =
  if isHeading t && not (hasAttrL "id" attrs)
    then
      -- A bare heading (NOT the direct heading-child of a section — those
      -- are stripped out by `walkSectionChildren` before reaching here, so
      -- their id stays on the section). This path catches headings that the
      -- sectioniser left unwrapped, e.g. inside a `<blockquote>` / `<li>`:
      -- the Djot reference puts the auto-id directly on the `<hN>`
      -- (corpus blockquote-012).
      let raw  = autoId (childrenText cs)
          slug = if raw == ""
                   then disambiguateFrom 1 "s" seen
                   else disambiguate raw seen
          (seen2, cs') = walkNodeList (slug :: seen) cs
       in (seen2, Element t (attrs ++ [MkHAttr "id" (Str slug)]) cs')
    else
      let (seen1, attrs') =
            if t == "section" && not (hasAttrL "id" attrs)
              then
                let raw  = autoId (sectionHeadingText cs)
                    slug = if raw == ""
                             then disambiguateFrom 1 "s" seen
                             else disambiguate raw seen
                 in (slug :: seen, attrs ++ [MkHAttr "id" (Str slug)])
              else
                -- Pre-existing id (section or otherwise): record it so
                -- later sections don't collide with it.
                case attrValueL "id" attrs of
                  Just v  => (v :: seen, attrs)
                  Nothing => (seen, attrs)
          -- A `<section>` walks its children with its OWN direct heading
          -- exempted from id-assignment (the id already rode onto the
          -- section); every other context walks children normally.
          (seen2, cs') = if t == "section"
                           then walkSectionChildren True seen1 cs
                           else walkNodeList seen1 cs
       in (seen2, Element t attrs' cs')

-- Walk a section's children. The first direct `<hN>` child is the
-- section's own heading — leave it id-less (the id is on the section) and
-- recurse into ITS inline children. `skipHeading` flips to `False` once
-- that heading has been passed so any later heading (a malformed sibling)
-- is treated normally.
walkSectionChildren _ seen [] = (seen, [])
walkSectionChildren skip seen (h :: hs) =
  case h of
    Element t a cs =>
      if skip && isHeading t
        then let (seen1, cs')  = walkNodeList seen cs
                 (seen2, hs')  = walkSectionChildren False seen1 hs
              in (seen2, Element t a cs' :: hs')
        else let (seen1, h')   = walkNode seen h
                 (seen2, hs')  = walkSectionChildren skip seen1 hs
              in (seen2, h' :: hs')
    _ =>
      let (seen1, h')  = walkNode seen h
          (seen2, hs') = walkSectionChildren skip seen1 hs
       in (seen2, h' :: hs')

||| Decorate every unanchored `<section>` with an `id="<autoId>"`
||| attribute derived from its heading text. Idempotent — re-running on an
||| already-anchored tree is a no-op. Existing ids (on sections or any
||| other element) are honoured and never overwritten; a section whose
||| heading text is empty falls back to `s` (disambiguated).
public export
addSectionIds : HExpr -> HExpr
addSectionIds h = snd (walkNode [] h)

--------------------------------------------------------------------------------
-- harvestHeadings: pre-order list of (level, anchor, plainTitle).
--------------------------------------------------------------------------------

||| Level + plain title of a section's direct heading child.
directHeading : List HExpr -> (Nat, String)
directHeading []                       = (0, "")
directHeading (Element t _ cs :: rest) = case headingLevel t of
  Just lvl => (lvl, childrenText cs)
  Nothing  => directHeading rest
directHeading (_ :: rest)              = directHeading rest

||| Walk the tree in document order; emit one row per `<section>`. The
||| anchor is the section's `id` (call `addSectionIds` first to fill in
||| the auto-ids); the level + title come from the section's heading.
public export
harvestHeadings : HExpr -> List (Nat, String, String)
harvestHeadings (Text _)    = []
harvestHeadings (Comment _) = []
harvestHeadings (Raw _)     = []
harvestHeadings (Element t attrs cs) =
  if t == "section"
    then
      let anchor       = case attrValueL "id" attrs of
                           Just v  => v
                           Nothing => ""
          (lvl, title) = directHeading cs
          here         = (lvl, anchor, title)
       in here :: concatMap (assert_total harvestHeadings) cs
    else concatMap (assert_total harvestHeadings) cs
