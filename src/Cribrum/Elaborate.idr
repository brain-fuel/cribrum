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
elaborateInline (InlImage _ _ xs)  =
  -- Image alt source = concatenated text of children; the full structural-AA
  -- check ("image must have an alt source") will land with Phase 4. For now
  -- the alt attribute is omitted in the spike emit; sites that demand alt
  -- can supply it via an attribute pass.
  Element "img" [] (assert_total (map elaborateInline xs))
elaborateInline (InlMath _ s)      = Element "code" [] [Text s]
elaborateInline (InlFootnoteRef l) = Text ("[" ++ l ++ "]")
elaborateInline (InlSymbol n)      = Text (":" ++ n ++ ":")
elaborateInline (InlRaw _ s)       = Text s
elaborateInline (InlSpan _ xs)     =
  Element "span" [] (assert_total (map elaborateInline xs))
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

public export
elaborateBlock : Block -> HExpr
elaborateBlock (Paragraph _ inlines) =
  Element "p" [] (map elaborateInline inlines)
elaborateBlock (Heading _ lvl inlines) =
  Element (headingTag lvl) [] (map elaborateInline inlines)
elaborateBlock (ThematicBreak _) =
  Element "hr" [] []
elaborateBlock (BlockQuote _ bs) =
  Element "blockquote" [] (assert_total (map elaborateBlock bs))
elaborateBlock (Div _ bs) =
  Element "div" [] (assert_total (map elaborateBlock bs))
elaborateBlock (CodeBlock _ _ body) =
  Element "pre" [] [Element "code" [] [Text body]]
elaborateBlock (RawBlock _ body) = Text body
elaborateBlock (ListBlock _ style _ _ items) =
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
      elabItem : ListItem -> HExpr
      elabItem i = Element "li" []
                     (assert_total (map elaborateBlock (content i)))
   in Element tag [] (map elabItem items)
elaborateBlock (Table _ _ _) =
  -- Table elaboration arrives with the table parser slice; emit empty table.
  Element "section" [] []
elaborateBlock (RefDef _ _ _) =
  -- Reference definitions don't render as visible blocks in Djot's HTML
  -- output; suppress as an empty comment.
  Comment "reference definition"
elaborateBlock (FootnoteDef _ l _) =
  Comment ("footnote: " ++ l)

--------------------------------------------------------------------------------
-- Top-level elaborate.
--------------------------------------------------------------------------------

||| Wrap the elaborated blocks in a single `<main>` landmark — a minimal
||| semantic root that satisfies the no-`div`-soup commitment. As Phase 1b
||| matures the wrapper will be inferred from heading structure.
public export
elaborateDoc : Doc -> HExpr
elaborateDoc (MkDoc bs) = Element "main" [] (map elaborateBlock bs)

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
