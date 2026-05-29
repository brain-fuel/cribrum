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
import public Cribrum.AA.Typed

%default total

--------------------------------------------------------------------------------
-- StructuralAA — Phase-4 conjunct.
--------------------------------------------------------------------------------

||| Structurally-decidable accessibility, per plan.dj §Phase 4: the conjunct
||| of every Structural rule from `Cribrum.AA.Catalog`, promoted to a type
||| via `Cribrum.AA.Typed`. A value of this type witnesses that the tree
||| passes all 10 propositions.
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

public export
Show ElabError where
  show (LocatedHtmlError lr) =
    "Elaboration produced invalid HTML: " ++ show lr
  show (InvalidProducedHtml t) =
    "Elaboration produced HTML with unknown tag: " ++ t
  show (StructuralAaFailure r p) =
    "Structural accessibility failure: " ++ r
      ++ case p of
           Nothing   => " (whole-tree)"
           Just path => " at " ++ show path

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
                                Yes p16 => Right (p1, p2, p3, p4, p5, p6, p7
                                                 , p8, p9, p10, p11
                                                 , p12, p13, p14, p15, p16)

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
elaborateInline (InlLink _ ref xs) =
  let inner = assert_total (map elaborateInline xs)
      attrs = case ref of
                LinkInline url _    => [MkHAttr "href" (Str url)]
                LinkReference label => [MkHAttr "href" (Str ("#" ++ label))]
                LinkAuto url        => [MkHAttr "href" (Str url)]
   in Element "a" attrs inner
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
   in Element "img" [ MkHAttr "alt" (Str alt)
                    , MkHAttr "src" (Str url)
                    ] []
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
elaborateInline (InlMath _ s)      = Element "code" [] [Text s]
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
elaborateBlock (ListBlock _ style start tight items) =
  let tag = case style of
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
          Element "ul" [MkHAttr "class" (Str "task-list")]
            (map elabTaskItem items)
        Definition =>
          Element "dl" [] (concatMap defPair items)
        _          =>
          let attrs = if tag == "ol" then olAttrs else []
           in Element tag attrs (map elabItem items)
elaborateBlock (Table _ _ rows) =
  -- Emit `<table>` with `<thead>` for header rows (set by the parser
  -- when an alignment row was present) and `<tbody>` for the body
  -- rows. When no row is a header, `<thead>` is omitted and all rows
  -- go into `<tbody>`.
  let headerRows = filter isHeader rows
      bodyRows   = filter (not . isHeader) rows
      headSection : List HExpr
      headSection = case headerRows of
        [] => []
        rs => [Element "thead" [] (map elabRow rs)]
      bodySection : List HExpr
      bodySection = case bodyRows of
        [] => []
        rs => [Element "tbody" [] (map elabRow rs)]
   in Element "table" [] (headSection ++ bodySection)
  where
    alignAttrs : Align -> List HAttr
    alignAttrs AlignNone   = []
    alignAttrs AlignLeft   = [MkHAttr "style" (Str "text-align:left")]
    alignAttrs AlignRight  = [MkHAttr "style" (Str "text-align:right")]
    alignAttrs AlignCenter = [MkHAttr "style" (Str "text-align:center")]

    elabCell : (cellTag : String) -> TableCell -> HExpr
    elabCell cellTag c =
      Element cellTag (alignAttrs (align c))
        (assert_total (map elaborateInline (content c)))

    elabRow : TableRow -> HExpr
    elabRow r =
      let cellTag = if isHeader r then "th" else "td"
       in Element "tr" [] (map (elabCell cellTag) (cells r))
elaborateBlock (RefDef _ _ _) =
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
isInvisibleBlock (RefDef _ _ _)      = True
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

||| Strict elaboration: returns the HExpr together with proofs of validity
||| and structural accessibility. Per plan.dj §Governing principle, callers
||| that demand `(h : HExpr ** IsValidHtml h × StructuralAA h)` are unable
||| to receive a malformed tree — the decision procedure manufactures the
||| witness or we return a located rejection.
|||
||| Failure ordering: HTML well-formedness checked first (located rejection),
||| then `StructuralAA` (rule id of first failing predicate). Both are hard
||| errors in strict mode.
public export
elaborate : Doc -> Either ElabError (h : HExpr ** (IsValidHtml h, StructuralAA h))
elaborate doc =
  let h = elaborateDoc doc
   in case decideHtmlLocated h of
        Left  lr => Left (LocatedHtmlError lr)
        Right p  => case decStructuralAA h of
          Left (rule, path) => Left (StructuralAaFailure rule path)
          Right aa          => Right (h ** (p, aa))
