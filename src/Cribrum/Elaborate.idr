||| Elaboration — Phase 1b per `plan.dj`.
|||
||| Maps Djot's surface AST to the single IR (HExpr), promoting div/span/class
||| soup into semantic HTML elements. Strict mode (the default) is total into
|||
|||   (h : HExpr ** IsValidHtml h × StructuralAA h)
|||
||| so a document that cannot become valid, accessible HTML is a *hard error*
||| — inaccessible documents are unrepresentable.
|||
||| `StructuralAA` is the conjunct of every Phase-4 rule in
||| `Cribrum.AA.Typed` — the 10 Structural rules from `Cribrum.AA.Catalog`
||| (img-alt, anchor-href, iframe-title, label-for-control, fieldset-legend,
||| button-name, link-name, document-lang, heading-no-skip, duplicate-id).
||| Each per-rule conjunct is decided by its own `decXxx`; a single
||| failure short-circuits to `StructuralAaFailure ruleId path`.
|||
||| AA failure locating (plan §P4.3 "each hard error is located"):
||| per-node rules carry `Just path` via `Promote.pathOfFirstFailing`;
||| the root-only `document-lang` carries `Just []`; whole-tree rules
||| whose failure isn't localised to a single node (heading-no-skip,
||| duplicate-id) carry `Nothing`.
module Cribrum.Elaborate

import Data.List
import public Data.List.Quantifiers
import public Data.So
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Html.Valid
import Cribrum.Html.Model
import Cribrum.Html.Category
import public Cribrum.AA.Typed

%default total

--------------------------------------------------------------------------------
-- StructuralAA — Phase-4 conjunct.
--------------------------------------------------------------------------------

||| Structurally-decidable accessibility, per plan.dj §Phase 4: the conjunct
||| of every Structural rule from `Cribrum.AA.Catalog`, promoted to a type
||| via `Cribrum.AA.Typed`. A value of this type witnesses that the tree
||| passes all 27 propositions.
public export
StructuralAA : HExpr -> Type
StructuralAA h =
  ( ImgsAllOk          h
  , AnchorsAllOk       h
  , IframesAllOk       h
  , LabelsAllOk        h
  , FieldsetsAllOk     h
  , ButtonsAllOk       h
  , LinksAllOk         h
  , DocumentLangOk     h
  , HeadingNoSkipOk    h
  , DuplicateIdOk      h
  , UniqueMainOk       h
  , AreasAllOk         h
  , LinkEmptyHrefAllOk h
  , MetaNoRefreshAllOk h
  , SummariesAllOk     h
  , TracksAllOk        h
  , InputImagesAllOk   h
  , ObjectNamesAllOk   h
  , ThScopesAllOk      h
  , ThHasNamesAllOk    h
  , NoEmptyHeadingsAllOk   h
  , SelectHasOptionsAllOk  h
  , CaptionFirstChildAllOk h
  , InputButtonNamesAllOk  h
  , AriaHiddenBodyAllOk    h
  , AriaRolesAllOk         h
  , AutocompletesAllOk     h
  )

--------------------------------------------------------------------------------
-- Elaboration errors.
--------------------------------------------------------------------------------

public export
data ElabError : Type where
  ||| Produced HExpr fails HTML well-formedness — Phase 2's content-model
  ||| check. Carries the located rejection (path-into-tree + reason class)
  ||| so consumers can pinpoint the offending node.
  LocatedHtmlError    : LocatedReject -> ElabError
  ||| Legacy spike constructor. Kept for backward compatibility with
  ||| any consumer that pattern-matched on the spike's `ElabError`.
  ||| New failures land in `LocatedHtmlError`.
  InvalidProducedHtml : (offendingTag : String) -> ElabError
  ||| Structural-AA failure — the produced HExpr is valid HTML but fails
  ||| one of the Phase-4 promoted rules. `rule` is the stable rule id
  ||| from `Cribrum.AA.Catalog` (e.g. `"img-alt"`, `"heading-no-skip"`).
  ||| `path` locates the offending node for per-node rules (img-alt,
  ||| anchor-href, iframe-title, label-for-control, fieldset-legend,
  ||| button-name, link-name); `Just []` for the root-only document-lang;
  ||| `Nothing` for whole-tree rules whose failure isn't localised to one
  ||| node (heading-no-skip, duplicate-id, unique-main).
  StructuralAaFailure : (rule : String) -> (path : Maybe (List Nat)) -> ElabError

||| Join strings with a separator (unambiguous; avoids `concat` overload
||| resolution on `String`/`List`).
joinStr : (sep : String) -> List String -> String
joinStr _   []        = ""
joinStr sep (x :: xs) = foldl (\acc, s => acc ++ sep ++ s) x xs

||| Render a tree path for a diagnostic. `[]` is the document root; a
||| non-empty path is the 0-indexed child route from the root.
public export
showElabPath : List Nat -> String
showElabPath [] = "the document root"
showElabPath p  = "child path " ++ show p ++ " (root -> "
                    ++ joinStr " -> " (map (("child " ++) . show) p)
                    ++ ")"

||| Human-readable, actionable explanation of a content-model rejection.
||| The structural cases that already read clearly fall back to `Show`.
public export
explainReject : RejectionClass -> String
explainReject (DisallowedAttr tag attr) =
  "attribute `" ++ attr ++ "` is not a valid HTML attribute on <" ++ tag
    ++ "> — it is not a global attribute, nor an aria-*, data-*, or on* "
    ++ "event handler. Remove it, or rename it to `data-" ++ attr ++ "`."
explainReject (InteractiveInInteractive anc child) =
  "<" ++ child ++ "> is interactive content and may not be nested inside <"
    ++ anc ++ ">. Unwrap it, or move it out of the <" ++ anc ++ ">."
explainReject (UnknownTag t) =
  "<" ++ t ++ "> is not a known HTML element."
explainReject (IllegalChild p c) =
  "<" ++ c ++ "> is not allowed directly inside <" ++ p ++ ">."
explainReject (BlockInPhrasing p c) =
  "<" ++ c ++ "> is block-level content and cannot appear inside the "
    ++ "phrasing-only element <" ++ p ++ ">."
explainReject other = show other

||| One-line meaning of a structural-AA rule id, for diagnostics.
public export
explainRule : String -> String
explainRule "anchor-href"     = " — every <a> must carry an href"
explainRule "link-empty-href" = " — an <a href=\"\"> is ineffective; give it a real destination"
explainRule "img-alt"         = " — every <img> needs an alt"
explainRule "link-name"       = " — every <a href> needs accessible text"
explainRule "button-name"     = " — every <button> needs an accessible name"
explainRule _                 = ""

public export
Show ElabError where
  show (LocatedHtmlError lr) =
    "invalid HTML at " ++ showElabPath (path lr) ++ ": " ++ explainReject (reason lr)
  show (InvalidProducedHtml t) =
    "Elaboration produced HTML with unknown tag: " ++ t
  show (StructuralAaFailure r p) =
    "structural accessibility failure (" ++ r ++ ")" ++ explainRule r
      ++ case p of
           Nothing   => " — affecting the whole document"
           Just path => " at " ++ showElabPath path

--------------------------------------------------------------------------------
-- Deciding the StructuralAA conjunct.
--------------------------------------------------------------------------------

||| Decide the full StructuralAA conjunct. On failure, return the rule id
||| of the first failing predicate (in catalog order) together with the
||| path-into-tree of the offending node (per-node rules), `Just []` for
||| the root-only document-lang, or `Nothing` for whole-tree rules whose
||| failure isn't localised. On success, return the conjunct witness.
public export
decStructuralAA :  (h : HExpr)
                -> Either (String, Maybe (List Nat)) (StructuralAA h)
decStructuralAA h = case decImgsAllOk h of
  No  _  => Left ("img-alt",            pathOfFirstFailing imgOkBool      h)
  Yes p1 => case decAnchorsAllOk h of
    No  _  => Left ("anchor-href",      pathOfFirstFailing anchorOkBool   h)
    Yes p2 => case decIframesAllOk h of
      No  _  => Left ("iframe-title",   pathOfFirstFailing iframeOkBool   h)
      Yes p3 => case decLabelsAllOk h of
        No  _  => Left ("label-for-control",
                                        pathOfFirstFailing labelOkBool    h)
        Yes p4 => case decFieldsetsAllOk h of
          No  _  => Left ("fieldset-legend",
                                        pathOfFirstFailing fieldsetOkBool h)
          Yes p5 => case decButtonsAllOk h of
            No  _  => Left ("button-name",
                                        pathOfFirstFailing buttonOkBool   h)
            Yes p6 => case decLinksAllOk h of
              No  _  => Left ("link-name",
                                        pathOfFirstFailing linkOkBool     h)
              Yes p7 => case decDocumentLangOk h of
                No  _  => Left ("document-lang", Just [])
                Yes p8 => case decHeadingNoSkipOk h of
                  No  _  => Left ("heading-no-skip",  Nothing)
                  Yes p9 => case decDuplicateIdOk h of
                    No  _   => Left ("duplicate-id",  Nothing)
                    Yes p10 => case decUniqueMainOk h of
                      No  _   => Left ("unique-main", Nothing)
                      Yes p11 => case decAreasAllOk h of
                        No  _   => Left ("area-alt",
                                          pathOfFirstFailing areaOkBool h)
                        Yes p12 => case decLinkEmptyHrefAllOk h of
                          No  _   => Left ("link-empty-href",
                                            pathOfFirstFailing linkEmptyHrefOkBool h)
                          Yes p13 => case decMetaNoRefreshAllOk h of
                            No  _   => Left ("meta-no-refresh",
                                              pathOfFirstFailing metaNoRefreshOkBool h)
                            Yes p14 => case decSummariesAllOk h of
                              No  _   => Left ("summary-not-empty",
                                                pathOfFirstFailing summaryOkBool h)
                              Yes p15 => case decTracksAllOk h of
                                No  _   => Left ("track-kind",
                                                  pathOfFirstFailing trackOkBool h)
                                Yes p16 => case decInputImagesAllOk h of
                                  No  _   => Left ("input-image-alt",
                                                    pathOfFirstFailing inputImageOkBool h)
                                  Yes p17 => case decObjectNamesAllOk h of
                                    No  _   => Left ("object-name",
                                                      pathOfFirstFailing objectNameOkBool h)
                                    Yes p18 => case decThScopesAllOk h of
                                      No  _   => Left ("th-scope-valid",
                                                        pathOfFirstFailing thScopeOkBool h)
                                      Yes p19 => case decThHasNamesAllOk h of
                                        No  _   => Left ("th-has-name",
                                                          pathOfFirstFailing thHasNameOkBool h)
                                        Yes p20 => case decNoEmptyHeadingsAllOk h of
                                          No  _   => Left ("no-empty-heading",
                                                            pathOfFirstFailing noEmptyHeadingOkBool h)
                                          Yes p21 => case decSelectHasOptionsAllOk h of
                                            No  _   => Left ("select-has-options",
                                                              pathOfFirstFailing selectHasOptionsOkBool h)
                                            Yes p22 => case decCaptionFirstChildAllOk h of
                                              No  _   => Left ("caption-first-child",
                                                                pathOfFirstFailing captionFirstChildOkBool h)
                                              Yes p23 => case decInputButtonNamesAllOk h of
                                                No  _   => Left ("input-button-name",
                                                                  pathOfFirstFailing inputButtonNameOkBool h)
                                                Yes p24 => case decAriaHiddenBodyAllOk h of
                                                  No  _   => Left ("aria-hidden-body",
                                                                    pathOfFirstFailing ariaHiddenBodyOkBool h)
                                                  Yes p25 => case decAriaRolesAllOk h of
                                                    No  _   => Left ("aria-role-valid",
                                                                      pathOfFirstFailing ariaRoleOkBool h)
                                                    Yes p26 => case decAutocompletesAllOk h of
                                                      No  _   => Left ("autocomplete-valid",
                                                                        pathOfFirstFailing autocompleteOkBool h)
                                                      Yes p27 => Right (p1, p2, p3, p4, p5, p6, p7
                                                                       , p8, p9, p10, p11
                                                                       , p12, p13, p14, p15, p16
                                                                       , p17, p18, p19, p20, p21
                                                                       , p22, p23, p24, p25, p26, p27)

||| Convert a Djot `Attrs` into the HTML attribute list. Emission
||| order (matching the reference Djot renderer):
|||
|||   class=... (joined by spaces, in source order, no dedupe)
|||   id=...    (if present)
|||   <other pairs in source order, last-value-wins per key>
|||
||| `class` / `id` keys appearing in `pairs` are folded into the
||| structured `classes` / `identifier` fields rather than emitted
||| twice.
public export
attrsToHAttrs : Attrs -> List HAttr
attrsToHAttrs (MkAttrs ident classes pairs) =
  let classAttr : List HAttr
      classAttr = case classes of
        [] => []
        cs => [MkHAttr "class" (Str (joinWith " " cs))]
      idAttr : List HAttr
      idAttr = case ident of
        Just i  => [MkHAttr "id" (Str i)]
        Nothing => []
      others : List HAttr
      others = map mkPairAttr (dedupeLastWins pairs)
   in classAttr ++ idAttr ++ others
  where
    joinWith : String -> List String -> String
    joinWith _   []        = ""
    joinWith _   [x]       = x
    joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

    mkPairAttr : (String, String) -> HAttr
    mkPairAttr (k, v) = MkHAttr k (Str v)

    -- Keep the LAST occurrence of each key, preserving the original
    -- relative order of distinct keys (first-seen position).
    dedupeLastWins : List (String, String) -> List (String, String)
    dedupeLastWins ps =
      let keys = nub (map fst ps)
       in mapMaybe (\k => map (\v => (k, v)) (lookupLast k ps)) keys
    where
      lookupLast : String -> List (String, String) -> Maybe String
      lookupLast _ []                = Nothing
      lookupLast k ((k', v) :: rest) =
        case lookupLast k rest of
          Just v' => Just v'
          Nothing => if k == k' then Just v else Nothing

||| Class names that promote an inline `[..]{.cls}` span to a semantic
||| phrasing element (convention catalog §2, span side — the
||| no-`span`-soup commitment). The inline mirror of `divConventionTag`:
||| the matched class is the authoring hint and is CONSUMED by
||| `promoteSpan`, so it never leaks into the emitted `class` attribute.
spanConventionTag : String -> Maybe String
spanConventionTag "abbr" = Just "abbr"
spanConventionTag "cite" = Just "cite"
spanConventionTag "dfn"  = Just "dfn"
spanConventionTag "kbd"  = Just "kbd"
spanConventionTag "samp" = Just "samp"
spanConventionTag "var"  = Just "var"
spanConventionTag "time" = Just "time"
spanConventionTag "q"    = Just "q"
spanConventionTag _      = Nothing

||| Resolve an inline span's class list to its emitted phrasing tag.
||| The FIRST convention class (source order) drives promotion and is
||| dropped from the returned residual class list; non-convention
||| classes are preserved in order. With no convention class the span
||| stays a plain `<span>` and all classes survive.
promoteSpan : List String -> (String, List String)
promoteSpan []        = ("span", [])
promoteSpan (c :: cs) = case spanConventionTag c of
  Just tag => (tag, cs)
  Nothing  => let (tag, cs') = promoteSpan cs in (tag, c :: cs')

--------------------------------------------------------------------------------
-- Inline elaboration.
--------------------------------------------------------------------------------

||| Emit a `title=…` HTML attribute when a link/image destination
||| carries one (`[ref]: url "title"`, or a `{title=…}` attribute), else
||| nothing. Factored out so both the link and image elaborators share
||| the same emission.
titleHAttr : Maybe String -> List HAttr
titleHAttr (Just t) = [MkHAttr "title" (Str t)]
titleHAttr Nothing  = []

||| Elaborate one inline. SoftBreak/HardBreak materialise as text " " and
||| `<br>` respectively, matching standard Djot HTML output. Constructs not
||| yet in the elaborator's slice (emphasis, links, ...) fall through to
||| their text content for now and will be replaced as the slice grows.
public export
elaborateInline : Inline -> HExpr
elaborateInline (InlText s)        = Text s
elaborateInline InlSoftBreak       = Text " "
elaborateInline InlHardBreak       = Element "br" [] []
elaborateInline (InlComment s)     = Comment s
elaborateInline (InlEmph xs)       =
  Element "em" [] (assert_total (map elaborateInline xs))
elaborateInline (InlStrong xs)     =
  Element "strong" [] (assert_total (map elaborateInline xs))
elaborateInline (InlHighlight xs)  =
  Element "mark" [] (assert_total (map elaborateInline xs))
elaborateInline (InlSuper xs)      =
  Element "sup" [] (assert_total (map elaborateInline xs))
elaborateInline (InlSub xs)        =
  Element "sub" [] (assert_total (map elaborateInline xs))
elaborateInline (InlInsert xs)     =
  Element "ins" [] (assert_total (map elaborateInline xs))
elaborateInline (InlDelete xs)     =
  Element "del" [] (assert_total (map elaborateInline xs))
elaborateInline (InlVerbatim _ s)  = Element "code" [] [Text s]
elaborateInline (InlLink linkAttrs ref xs) =
  let inner = assert_total (map elaborateInline xs)
      -- Destination → href. An empty destination is left empty here; the
      -- strict `elaborate` pass fills it with "#" before the structural
      -- gate (bucket C: links-and-images-007 renders `<a href="#">`
      -- instead of erroring), while draft mode repairs + warns. Keeping
      -- the empty href visible at this layer preserves both paths.
      -- Unresolved reference labels surface as `#<label>` so the
      -- dangling reference stays visible.
      href : String
      href = case ref of
               LinkInline url _    => url
               LinkReference label => "#" ++ label
               LinkAuto url        => url
      -- A reference definition's `title` (from `[ref]: url "t"` or a
      -- `{title=…}` attribute block preceding the def — bucket A) flows
      -- onto the resolved link. The link's OWN inline `{…}` attributes
      -- override it (links-and-images-014: `{title=bar}` beats the
      -- def's `foo`); any non-title author attrs ride through the
      -- validity gate via `attrsToHAttrs`.
      refTitle : Maybe String
      refTitle = case ref of
                   LinkInline _ t => t
                   _              => Nothing
      lpairs : List (String, String)
      lpairs = pairs linkAttrs
      title : List HAttr
      title = case lookup "title" lpairs of
                Just t  => [MkHAttr "title" (Str t)]
                Nothing => titleHAttr refTitle
      otherAttrs : List HAttr
      otherAttrs = attrsToHAttrs
                     ({ pairs := filter (\p => fst p /= "title") lpairs }
                              linkAttrs)
   in Element "a" (MkHAttr "href" (Str href) :: title ++ otherAttrs) inner
elaborateInline (InlImage _ ref xs) =
  -- `<img>` is a void element, so children are not legal HExpr content;
  -- instead the alt source (concatenated plain text of the parsed alt
  -- inlines) becomes the `alt` attribute and the link ref becomes the
  -- `src` attribute. Empty alt (`![](url)`) is permitted by the
  -- structural img-alt rule (decorative image — WCAG-conformant), so
  -- the attribute is always emitted regardless of content length.
  let alt = inlinesPlainText xs
      url = case ref of
              LinkInline u _    => u
              LinkReference l   => "#" ++ l
              LinkAuto u        => u
      titleAttr : List HAttr
      titleAttr = case ref of
                    LinkInline _ t => titleHAttr t
                    _              => []
   in Element "img" ([ MkHAttr "alt" (Str alt)
                     , MkHAttr "src" (Str url)
                     ] ++ titleAttr) []
  where
    -- Flatten the alt inlines to a plain-text string. Structural
    -- markers (emphasis, strong, verbatim, links) contribute their
    -- text children; hard/soft breaks contribute a single space; the
    -- handful of leaf forms that aren't text (footnote refs, smart
    -- punct, etc.) contribute their printed representation so the
    -- alt source is at least non-empty when authors intended it.
    inlinesPlainText : List Inline -> String
    inlinesPlainText is = concat (assert_total (map oneText is))
      where
        oneText : Inline -> String
        oneText (InlText s)        = s
        oneText InlSoftBreak       = " "
        oneText InlHardBreak       = " "
        oneText (InlComment _)     = ""
        oneText (InlEmph ys)       = inlinesPlainText ys
        oneText (InlStrong ys)     = inlinesPlainText ys
        oneText (InlHighlight ys)  = inlinesPlainText ys
        oneText (InlSuper ys)      = inlinesPlainText ys
        oneText (InlSub ys)        = inlinesPlainText ys
        oneText (InlInsert ys)     = inlinesPlainText ys
        oneText (InlDelete ys)     = inlinesPlainText ys
        oneText (InlVerbatim _ s)  = s
        oneText (InlLink _ _ ys)   = inlinesPlainText ys
        oneText (InlImage _ _ ys)  = inlinesPlainText ys
        oneText (InlMath _ s)      = s
        oneText (InlFootnoteRef l) = "[" ++ l ++ "]"
        oneText (InlSymbol n)      = ":" ++ n ++ ":"
        oneText (InlRaw _ s)       = s
        oneText (InlSpan _ ys)     = inlinesPlainText ys
        oneText (InlSmart sp)      = case sp of
          LDQuote  => "\x201C"
          RDQuote  => "\x201D"
          LSQuote  => "\x2018"
          RSQuote  => "\x2019"
          EnDash   => "\x2013"
          EmDash   => "\x2014"
          Ellipsis => "\x2026"
elaborateInline (InlMath isDisplay s) =
  -- Djot math (conventions §1, "Math" row): inline math is a `<span
  -- class="math inline">` wrapping the verbatim body in `\(…\)`; display
  -- math (`$$`) is `<span class="math display">` wrapping it in `\[…\]`.
  -- The body is NOT markup-parsed; it rides through as a `Text` node so the
  -- renderer entitises `<`/`>`/`&` (everything else, incl. `$`, is literal).
  let cls   = if isDisplay then "math display" else "math inline"
      body  = if isDisplay then "\\[" ++ s ++ "\\]" else "\\(" ++ s ++ "\\)"
   in Element "span" [MkHAttr "class" (Str cls)] [Text body]
elaborateInline (InlFootnoteRef l) =
  -- A footnote reference becomes a `<sup>` anchor targeting the
  -- `<aside class="footnote" id="fn-<label>">` emitted for the matching
  -- `FootnoteDef`. Label-anchored (no upstream-style renumbering); a
  -- ref with no definition is simply a dangling intra-document link.
  Element "a" [MkHAttr "href" (Str ("#fn-" ++ l))]
    [Element "sup" [] [Text l]]
elaborateInline (InlSymbol n)      = Text (":" ++ n ++ ":")
elaborateInline (InlRaw fmt s)     =
  -- Raw-inline gating (conventions §1, "Raw inline" row): the `html`
  -- format injects its content verbatim as a `Raw` passthrough node;
  -- every other format is suppressed (no output), matching the
  -- reference renderer which drops raw spans of unknown formats.
  if fmt == "html" then Raw s else Text ""
elaborateInline (InlSpan (MkAttrs ident classes pairs) xs) =
  -- Convention §2 (span side): a convention class promotes the span to a
  -- semantic phrasing element and is consumed; `{role=}`/`{lang=}` and the
  -- rest ride through. Previously the whole attribute block was dropped.
  let (tag, classes') = promoteSpan classes
   in Element tag (attrsToHAttrs (MkAttrs ident classes' pairs))
        (assert_total (map elaborateInline xs))
elaborateInline (InlSmart sp) = case sp of
  LDQuote  => Text "\x201C"   -- “
  RDQuote  => Text "\x201D"   -- ”
  LSQuote  => Text "\x2018"   -- ‘
  RSQuote  => Text "\x2019"   -- ’
  EnDash   => Text "\x2013"   -- –
  EmDash   => Text "\x2014"   -- —
  Ellipsis => Text "\x2026"   -- …

--------------------------------------------------------------------------------
-- Block elaboration.
--------------------------------------------------------------------------------

||| Heading level -> tag. Levels 1..6 go to h1..h6; anything else falls back
||| to h1 with a structural marker (this is a defensive choice — the parser
||| should never emit a level outside [1,6]; if it did, we still want a known
||| tag so IsValidHtml witnesses succeed).
headingTag : Nat -> String
headingTag 1 = "h1"
headingTag 2 = "h2"
headingTag 3 = "h3"
headingTag 4 = "h4"
headingTag 5 = "h5"
headingTag 6 = "h6"
headingTag _ = "h1"

||| Class names that promote a fenced `:::cls` div to a semantic
||| landmark / sectioning element (convention catalog §2 — the
||| no-`div`-soup commitment). The matched class is the authoring hint;
||| it is CONSUMED by `promoteDiv` so it never leaks into the emitted
||| `class` attribute. `main`/`section` are accepted as explicit
||| overrides of the structural inference in §3.
divConventionTag : String -> Maybe String
divConventionTag "nav"        = Just "nav"
divConventionTag "aside"      = Just "aside"
divConventionTag "figure"     = Just "figure"
divConventionTag "figcaption" = Just "figcaption"
divConventionTag "header"     = Just "header"
divConventionTag "footer"     = Just "footer"
divConventionTag "section"    = Just "section"
divConventionTag "main"       = Just "main"
divConventionTag _            = Nothing

||| Resolve a fenced div's class list to its emitted element tag.
||| The FIRST convention class (source order) drives promotion and is
||| dropped from the returned residual class list; non-convention
||| classes are preserved in order. With no convention class the div
||| stays a plain `<div>` and all classes survive.
promoteDiv : List String -> (String, List String)
promoteDiv []        = ("div", [])
promoteDiv (c :: cs) = case divConventionTag c of
  Just tag => (tag, cs)
  Nothing  => let (tag, cs') = promoteDiv cs in (tag, c :: cs')

public export
elaborateBlock : Block -> HExpr
elaborateBlock (Paragraph a inlines) =
  Element "p" (attrsToHAttrs a) (map elaborateInline inlines)
elaborateBlock (Heading a lvl inlines) =
  Element (headingTag lvl) (attrsToHAttrs a) (map elaborateInline inlines)
elaborateBlock (ThematicBreak a) =
  Element "hr" (attrsToHAttrs a) []
elaborateBlock (BlockQuote a bs) =
  Element "blockquote" (attrsToHAttrs a)
    (assert_total (map elaborateBlock bs))
elaborateBlock (Div (MkAttrs ident classes pairs) bs) =
  -- Convention §2: a convention class promotes the div to a semantic
  -- element and is consumed; `{role=}`/`{lang=}` ride through as pairs.
  let (tag, classes') = promoteDiv classes
   in Element tag (attrsToHAttrs (MkAttrs ident classes' pairs))
        (assert_total (map elaborateBlock bs))
elaborateBlock (CodeBlock a info body) =
  let codeAttrs : List HAttr
      codeAttrs = case info of
        "" => []
        i  => [MkHAttr "class" (Str ("language-" ++ i))]
   in Element "pre" (attrsToHAttrs a) [Element "code" codeAttrs [Text body]]
elaborateBlock (RawBlock fmt body) =
  -- Raw-block gating (conventions §1, "Raw block" row): `=html` fenced
  -- blocks inject their body verbatim as a `Raw` passthrough node; raw
  -- blocks tagged with any other format are suppressed (no output).
  if fmt == "html" then Raw body else Text ""
elaborateBlock (ListBlock a style start tight items) =
  let listAttrs = attrsToHAttrs a
      tag : String
      tag = case style of
              OrderedDecimal     => "ol"
              OrderedRomanLower  => "ol"
              OrderedRomanUpper  => "ol"
              OrderedAlphaLower  => "ol"
              OrderedAlphaUpper  => "ol"
              UnorderedDash      => "ul"
              UnorderedAsterisk  => "ul"
              UnorderedPlus      => "ul"
              TaskList           => "ul"
              Definition         => "dl"
      -- Ordered lists emit a `type=` attribute for non-decimal number
      -- styles (`a`/`A`/`i`/`I`) and a `start=` attribute when the first
      -- marker is not 1. The reference renderer omits both for plain
      -- decimal lists starting at 1.
      typeAttr : List HAttr
      typeAttr = case style of
        OrderedRomanLower => [MkHAttr "type" (Str "i")]
        OrderedRomanUpper => [MkHAttr "type" (Str "I")]
        OrderedAlphaLower => [MkHAttr "type" (Str "a")]
        OrderedAlphaUpper => [MkHAttr "type" (Str "A")]
        _                 => []
      startAttr : List HAttr
      startAttr = case start of
        Just n  => [MkHAttr "start" (Str (show n))]
        Nothing => []
      olAttrs : List HAttr
      olAttrs = startAttr ++ typeAttr
      -- Tight list collapse: in a tight list the reference renderer
      -- emits a `<li>`'s paragraph content inline (no `<p>` wrap), while
      -- non-paragraph children (e.g. a nested sub-list) render normally.
      -- A tight item is thus the flattened concatenation of: inline
      -- content for each direct `Paragraph`, and the elaborated form of
      -- every other block. TaskList items are always treated as tight.
      -- Loose items keep their `<p>` wrap (this helper is not used).
      unwrapTight : List Block -> List HExpr
      unwrapTight []                       = []
      unwrapTight (Paragraph _ inls :: bs) =
        map elaborateInline inls ++ assert_total (unwrapTight bs)
      unwrapTight (b :: bs)                =
        assert_total (elaborateBlock b) :: assert_total (unwrapTight bs)

      -- TaskList items carry `checked = Just bool`. The reference
      -- renderer adds a `class="checked"` / `"unchecked"` attribute
      -- on the `<li>` itself rather than emitting a checkbox input.
      taskLiAttrs : Maybe Bool -> List HAttr
      taskLiAttrs (Just True)  = [MkHAttr "class" (Str "checked")]
      taskLiAttrs (Just False) = [MkHAttr "class" (Str "unchecked")]
      taskLiAttrs Nothing      = []

      elabItem : ListItem -> HExpr
      elabItem i =
        if tight
          then Element "li" [] (unwrapTight (content i))
          else Element "li" []
                 (assert_total (map elaborateBlock (content i)))

      elabTaskItem : ListItem -> HExpr
      elabTaskItem i =
        Element "li" (taskLiAttrs (checked i))
          (if tight
             then unwrapTight (content i)
             else assert_total (map elaborateBlock (content i)))

      -- Definition items decompose into one `<dt>` (term) and an
      -- optional `<dd>` (body) sibling pair. Items with no body emit
      -- just the `<dt>`. Body blocks elaborate normally so multi-
      -- paragraph definitions / nested lists round-trip.
      defPair : ListItem -> List HExpr
      defPair i =
        let termInls = case term i of
                         Just ts => ts
                         Nothing => []
            dt = Element "dt" [] (map elaborateInline termInls)
            dd = case content i of
                   [] => []
                   bs => [Element "dd" []
                            (assert_total (map elaborateBlock bs))]
         in dt :: dd

   in case style of
        TaskList   =>
          Element "ul" (listAttrs ++ [MkHAttr "class" (Str "task-list")])
            (map elabTaskItem items)
        Definition =>
          Element "dl" listAttrs (concatMap defPair items)
        _          =>
          let attrs = listAttrs ++ (if tag == "ol" then olAttrs else [])
           in Element tag attrs (map elabItem items)
elaborateBlock (Table _ cap rows) =
  -- Djot reference output places `<tr>` rows directly inside `<table>`
  -- (no `<thead>`/`<tbody>` wrappers), preserving source order. Header
  -- rows (flagged by the parser from a following alignment row) emit
  -- `<th>` cells; body rows emit `<td>`. An optional `<caption>` (from
  -- a trailing `^ ...` caption block) leads the table.
  let captionSection : List HExpr
      captionSection = case cap of
        Nothing => []
        Just is => [Element "caption" []
                      (assert_total (map elaborateInline is))]
   in Element "table" [] (captionSection ++ map elabRow rows)
  where
    -- Reference renderer formats the alignment as `text-align: <dir>;`
    -- (space after the colon, trailing semicolon).
    alignAttrs : Align -> List HAttr
    alignAttrs AlignNone   = []
    alignAttrs AlignLeft   = [MkHAttr "style" (Str "text-align: left;")]
    alignAttrs AlignRight  = [MkHAttr "style" (Str "text-align: right;")]
    alignAttrs AlignCenter = [MkHAttr "style" (Str "text-align: center;")]

    elabCell : (cellTag : String) -> TableCell -> HExpr
    elabCell cellTag c =
      Element cellTag (alignAttrs (align c))
        (assert_total (map elaborateInline (content c)))

    elabRow : TableRow -> HExpr
    elabRow r =
      let cellTag = if isHeader r then "th" else "td"
       in Element "tr" [] (map (elabCell cellTag) (cells r))
elaborateBlock (RefDef _ _ _ _) =
  -- Reference definitions don't render as visible blocks in Djot's HTML
  -- output; suppress as an empty comment.
  Comment "reference definition"
elaborateBlock (FootnoteDef _ l bs) =
  -- Convention §1: a footnote definition becomes a semantic
  -- `<aside class="footnote">`, anchored by label (`id="fn-<label>"`)
  -- so the matching `InlFootnoteRef` anchor can target it. Cribrum's
  -- label-anchored model deliberately diverges from upstream Djot's
  -- numbered `<section role="doc-endnotes">` collection — see the
  -- footnote rows in docs/conventions.md §1.
  Element "aside"
    [ MkHAttr "class" (Str "footnote")
    , MkHAttr "id" (Str ("fn-" ++ l))
    ]
    (assert_total (map elaborateBlock bs))

--------------------------------------------------------------------------------
-- Top-level elaborate.
--------------------------------------------------------------------------------

||| `True` for blocks that contribute *no* visible output in the
||| rendered document. Reference definitions are structural markers
||| consumed by the inline-link resolver — the reference Djot renderer
||| emits nothing for them, and so does Cribrum (an injected HTML
||| comment would break exact-match conformance against the reference
||| suite). Footnote definitions, by contrast, now elaborate to a
||| visible `<aside class="footnote">` (convention §1) and are NOT
||| filtered here.
isInvisibleBlock : Block -> Bool
isInvisibleBlock (RefDef _ _ _ _)    = True
isInvisibleBlock _                   = False

||| Heading level of a block, if it is a `Heading`.
headingBlockLevel : Block -> Maybe Nat
headingBlockLevel (Heading _ lvl _) = Just lvl
headingBlockLevel _                 = Nothing

||| Pull the `id` attribute off an element's attr list, returning the id
||| value (if any) and the remaining attrs. Used to MOVE a heading's
||| explicit `{#id}` onto its wrapping `<section>` so the heading itself
||| carries no id (Djot headings-013/015 shape).
extractIdAttr : List HAttr -> (Maybe String, List HAttr)
extractIdAttr []                              = (Nothing, [])
extractIdAttr (MkHAttr "id" (Str v) :: rest)  = (Just v, rest)
extractIdAttr (a :: rest)                     =
  let (mid, rest') = extractIdAttr rest in (mid, a :: rest')

||| Wrap heading-led runs of a flat block list into nested `<section>`
||| landmarks — plan.dj §1b "heading-level sequences are inferred into
||| nested `<section>` structure". A heading at level L opens a section
||| holding the heading plus every following block up to (but not
||| including) the next heading of level <= L; deeper headings nest as
||| child sections. Blocks before the first heading stay unwrapped at the
||| top level. A heading's explicit id moves onto its section. Auto-ids
||| for id-less sections are filled in afterwards by
||| `Cribrum.Pipeline.Anchor.addSectionIds`, kept out of the strict
||| codomain so the disambiguator can never introduce a duplicate id.
public export
sectionize : List Block -> List HExpr
sectionize []        = []
sectionize (b :: bs) = case headingBlockLevel b of
  Nothing  => elaborateBlock b :: assert_total (sectionize bs)
  Just lvl =>
    let (inside, after) = break (closesSection lvl) bs
        (secId, hdr)    = case elaborateBlock b of
          Element t attrs cs =>
            let (mid, attrs') = extractIdAttr attrs
             in (mid, Element t attrs' cs)
          other => (Nothing, other)
        secAttrs        = case secId of
          Just i  => [MkHAttr "id" (Str i)]
          Nothing => []
        children        = hdr :: assert_total (sectionize inside)
     in Element "section" secAttrs children
          :: assert_total (sectionize after)
  where
    ||| A heading of level <= `lvl` closes the section opened at `lvl`.
    closesSection : Nat -> Block -> Bool
    closesSection lvl b' = case headingBlockLevel b' of
      Just l' => l' <= lvl
      Nothing => False

||| Wrap the elaborated blocks in a single `<main>` landmark, with
||| heading-level sequences inferred into nested `<section>` structure
||| (`sectionize`). Satisfies the no-`div`-soup commitment with a real
||| semantic root + landmark sectioning.
|||
||| Convention §3 "inference with override": the `<main>` wrapper is an
||| *inference*. When the document body already supplies its own `<main>`
||| landmark as its whole content, the explicit landmark wins and we emit
||| it directly — wrapping it again would produce two main landmarks and
||| fail the `unique-main` structural rule. Two authoring forms count as an
||| explicit main:
|||
|||   * a top-level `:::main` fenced div (`promoteDiv` → `<main>` tag);
|||   * a top-level element carrying `role="main"` (the ARIA main
|||     landmark, e.g. `:::{role=main}` → `<div role="main">`).
|||
||| Either as the document's whole content steps the wrapper aside. Mixed
||| main+sibling layouts stay deferred.
isMainLandmark : HExpr -> Bool
isMainLandmark (Element "main" _ _) = True
isMainLandmark (Element _ attrs _)  = any isRoleMain attrs
  where
    isRoleMain : HAttr -> Bool
    isRoleMain (MkHAttr "role" (Str "main")) = True
    isRoleMain _                             = False
isMainLandmark _                    = False

public export
elaborateDoc : Doc -> HExpr
elaborateDoc (MkDoc bs) =
  case sectionize (filter (not . isInvisibleBlock) bs) of
    [single] => if isMainLandmark single
                  then single
                  else Element "main" [] [single]
    children => Element "main" [] children

||| Force a usable `href` on an `<a>` whose attribute list has an
||| absent or empty href: every other href is left untouched. Used by
||| strict `elaborate` so an empty/missing destination renders as
||| `<a href="#">` (an effective, non-crashing default — bucket C,
||| links-and-images-007) instead of tripping the structural
||| `anchor-href` / `link-empty-href` gate. Draft mode keeps its own
||| repair-and-warn path; this strict default is silent.
fillEmptyHref : List HAttr -> List HAttr
fillEmptyHref attrs =
    if hasUsableHref attrs then attrs else setHref attrs
  where
    hasUsableHref : List HAttr -> Bool
    hasUsableHref []                            = False
    hasUsableHref (MkHAttr "href" (Str v) :: _) = v /= ""
    hasUsableHref (MkHAttr "href" _ :: _)       = True
    hasUsableHref (_ :: xs)                     = hasUsableHref xs

    setHref : List HAttr -> List HAttr
    setHref []                       = [MkHAttr "href" (Str "#")]
    setHref (MkHAttr "href" _ :: xs) = MkHAttr "href" (Str "#") :: xs
    setHref (a :: xs)                = a :: setHref xs

||| Walk the tree filling empty/missing `<a>` hrefs with `#` (strict
||| default; see `fillEmptyHref`). Non-anchor elements are walked
||| recursively but otherwise untouched.
fillHrefs : HExpr -> HExpr
fillHrefs (Element "a" attrs cs) =
  Element "a" (fillEmptyHref attrs) (assert_total (map fillHrefs cs))
fillHrefs (Element tag attrs cs)  =
  Element tag attrs (assert_total (map fillHrefs cs))
fillHrefs other                   = other

||| Strict elaboration: returns the HExpr together with proofs of validity
||| and structural accessibility. Per plan.dj §Governing principle, callers
||| that demand `(h : HExpr ** IsValidHtml h × StructuralAA h)` are unable
||| to receive a malformed tree — the decision procedure manufactures the
||| witness or we return a located rejection.
|||
||| Failure ordering: HTML well-formedness checked first (located rejection),
||| then `StructuralAA` (rule id of first failing predicate). Both are hard
||| errors in strict mode. Empty/absent anchor hrefs are filled with `#`
||| (`fillHrefs`) before the gate so a destination-less link renders rather
||| than erroring.
public export
elaborate : Doc -> Either ElabError (h : HExpr ** (IsValidHtml h, StructuralAA h))
elaborate doc =
  let h = fillHrefs (elaborateDoc doc)
   in case decideHtmlLocated h of
        Left  lr => Left (LocatedHtmlError lr)
        Right p  => case decStructuralAA h of
          Left (rule, path) => Left (StructuralAaFailure rule path)
          Right aa          => Right (h ** (p, aa))

--------------------------------------------------------------------------------
-- Draft mode: placeholder repair + actionable warnings.
--
-- Strict `elaborate` hard-errors on invalid / inaccessible output. Draft
-- mode instead *repairs* the common author mistakes into valid placeholder
-- nodes — each tagged with a `data-cribrum-todo` marker and reported as a
-- `DraftWarning` — so a work-in-progress document still renders
-- (proof-carrying) while loudly flagging what must be fixed. This is the
-- "clear first, then fix" workflow, akin to Rust's `todo!()`: the output
-- compiles/renders, but it is unmistakably unfinished.
--
-- Repaired patterns (the structurally-decidable author mistakes):
--   * disallowed attribute `x="v"`   -> renamed to `data-x="v"`   (1a)
--   * interactive element inside <a>  -> demoted to <span>         (1b)
--   * <a> with no href / empty href   -> href="#"                  (2a/2b)
--
-- Anything draft cannot repair (unknown tags, illegal nesting, duplicate
-- ids, heading skips, …) still hard-errors: draft repairs author slips,
-- it is not a "make any tree valid" escape hatch.
--------------------------------------------------------------------------------

||| A single thing the author must fix before the document is strict-clean.
public export
record DraftWarning where
  constructor MkDraftWarning
  ||| Path-into-tree of the repaired node (`[]` = root).
  path   : List Nat
  ||| Stable rule id the placeholder stands in for.
  rule   : String
  ||| What is wrong (the diagnosis).
  detail : String
  ||| What the author must do to fix it (the `todo!`).
  todo   : String

public export
Show DraftWarning where
  show w = "TODO[" ++ rule w ++ " @ " ++ showElabPath (path w) ++ "]: "
             ++ detail w ++ "  (fix: " ++ todo w ++ ")"

||| `True` iff `tag` is interactive content (per the element catalog).
draftIsInteractive : String -> Bool
draftIsInteractive t = elem Interactive (categoriesOf t)

||| Rename every attribute not permitted on `tag` to `data-<name>` (always
||| valid, value preserved). Returns the fixed attr list + the renamed
||| original names (for warnings).
draftFixAttrs : (tag : String) -> List HAttr -> (List HAttr, List String)
draftFixAttrs _   []                        = ([], [])
draftFixAttrs tag (a@(MkHAttr n v) :: rest) =
  let (kept, renamed) = draftFixAttrs tag rest
   in if isAllowedAttrName tag n
        then (a :: kept, renamed)
        else (MkHAttr ("data-" ++ n) v :: kept, n :: renamed)

||| `(hasHrefKey, hrefIsUsable)` for an attribute list.
draftHrefStatus : List HAttr -> (Bool, Bool)
draftHrefStatus []                             = (False, False)
draftHrefStatus (MkHAttr "href" (Str v) :: _)  = (True, v /= "")
draftHrefStatus (MkHAttr "href" _ :: _)        = (True, True)
draftHrefStatus (_ :: xs)                      = draftHrefStatus xs

||| Force `href="#"`, replacing any existing href (else appending one).
draftSetHref : List HAttr -> List HAttr
draftSetHref []                        = [MkHAttr "href" (Str "#")]
draftSetHref (MkHAttr "href" _ :: xs)  = MkHAttr "href" (Str "#") :: xs
draftSetHref (a :: xs)                 = a :: draftSetHref xs

||| Short combined id for the visible `data-cribrum-todo` marker.
draftMarkerId : (demote : Bool) -> (renamed : List String) -> (fixHref : Bool) -> String
draftMarkerId d r h =
  let parts : List String
      parts = (if d then ["interactive-in-interactive"] else [])
            ++ (if isNil r then [] else ["disallowed-attr"])
            ++ (if h then ["href"] else [])
   in case parts of
        [] => "fixme"
        ps => joinStr "+" ps

mutual
  ||| Repair one node: returns the (valid) placeholder node + warnings.
  ||| `inAnchor` tracks whether we are inside an `<a>` ancestor.
  draftRepair : (path : List Nat) -> (inAnchor : Bool) -> HExpr
             -> (HExpr, List DraftWarning)
  draftRepair _    _        (Text s)    = (Text s, [])
  draftRepair _    _        (Comment s) = (Comment s, [])
  draftRepair _    _        (Raw s)     = (Raw s, [])
  draftRepair path inAnchor (Element tag attrs cs) =
    let -- (1b) demote interactive content nested inside an <a>.
        demote  = inAnchor && draftIsInteractive tag
        tag1    = if demote then "span" else tag
        wDemote = if demote
                    then [MkDraftWarning path "interactive-in-interactive"
                           ("<" ++ tag ++ "> is interactive and cannot be nested inside an <a>")
                           ("unwrap it or move it out of the link "
                             ++ "(draft demoted it to <span>)")]
                    else []
        -- (1a) rename disallowed attributes (run against the final tag).
        (keptAttrs, renamed) = draftFixAttrs tag1 attrs
        wAttrs = map (\n => MkDraftWarning path "disallowed-attr"
                            ("attribute `" ++ n ++ "` is not valid HTML on <" ++ tag1 ++ ">")
                            ("remove it or make it a real attribute "
                              ++ "(draft renamed it to `data-" ++ n ++ "`)"))
                     renamed
        -- (2a/2b) anchors need a usable href.
        (hasHref, goodHref) = draftHrefStatus keptAttrs
        fixHref   = tag1 == "a" && not goodHref
        attrsHref = if fixHref then draftSetHref keptAttrs else keptAttrs
        wHref = if fixHref
                  then [MkDraftWarning path
                         (if hasHref then "link-empty-href" else "anchor-href")
                         (if hasHref then "this <a> has an empty href"
                                     else "this <a> has no href")
                         ("point it at a real destination (draft used href=\"#\")")]
                  else []
        touched = demote || not (isNil renamed) || fixHref
        attrsFinal = if touched
                       then attrsHref
                              ++ [MkHAttr "data-cribrum-todo"
                                    (Str (draftMarkerId demote renamed fixHref))]
                       else attrsHref
        inAnchor'   = tag1 == "a" || inAnchor
        (cs', wcs)  = draftRepairList path 0 inAnchor' cs
     in (Element tag1 attrsFinal cs', wDemote ++ wAttrs ++ wHref ++ wcs)

  draftRepairList : List Nat -> Nat -> Bool -> List HExpr
                 -> (List HExpr, List DraftWarning)
  draftRepairList _    _   _        []        = ([], [])
  draftRepairList path idx inAnchor (c :: cs) =
    let (c',  w1) = draftRepair (path ++ [idx]) inAnchor c
        (cs', w2) = draftRepairList path (S idx) inAnchor cs
     in (c' :: cs', w1 ++ w2)

||| Draft elaboration: repairs the structurally-decidable author mistakes
||| into valid placeholders and returns the proof-carrying tree alongside
||| the list of `DraftWarning`s describing what still must be fixed. Mistakes
||| draft cannot repair still surface as a hard `ElabError` (so draft is an
||| authoring aid, not a way to smuggle malformed HTML past the guarantees).
public export
elaborateDraft : Doc
              -> Either ElabError
                   (h : HExpr ** (IsValidHtml h, StructuralAA h, List DraftWarning))
elaborateDraft doc =
  let raw                  = elaborateDoc doc
      (repaired, warnings) = draftRepair [] False raw
   in case decideHtmlLocated repaired of
        Left  lr => Left (LocatedHtmlError lr)
        Right p  => case decStructuralAA repaired of
          Left (rule, path) => Left (StructuralAaFailure rule path)
          Right aa          => Right (repaired ** (p, aa, warnings))
