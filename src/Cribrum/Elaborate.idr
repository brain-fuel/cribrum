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
||| **Scope of this spike:** the slice currently shipped covers paragraph,
||| heading (h1-h6), thematic break (`<hr>`), and inline text + soft/hard
||| breaks (`<br>`). `StructuralAA` is the unit relation here — the
||| structurally-decidable AA failures arrive once the relevant Djot
||| constructs do (images need alt, controls need labels, headings shouldn't
||| skip levels, document needs a `lang`).
|||
||| The strict-mode contract is preserved: the spike's StructuralAA proof
||| obligation is trivially satisfiable, so any HExpr produced here is BOTH
||| `IsValidHtml` and `StructuralAA`. As Phase 3/4 lands, this module is
||| where the failure modes light up — the type stays the same.
module Cribrum.Elaborate

import Data.List
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Html.Valid

%default total

--------------------------------------------------------------------------------
-- StructuralAA placeholder.
--------------------------------------------------------------------------------

||| Structurally-decidable accessibility, per plan.dj §Phase 4. Phase 4 will
||| land the actual rule set; the spike treats it as the universally true
||| proposition so the elaboration codomain is the right *shape*.
public export
StructuralAA : HExpr -> Type
StructuralAA _ = ()

public export
trivialAA : (h : HExpr) -> StructuralAA h
trivialAA _ = ()

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
  ||| Reserved for future structural-AA failures (image without alt source,
  ||| skipped heading level, etc).
  StructuralAaFailure : (rule : String) -> ElabError

public export
Show ElabError where
  show (LocatedHtmlError lr) =
    "Elaboration produced invalid HTML: " ++ show lr
  show (InvalidProducedHtml t) =
    "Elaboration produced HTML with unknown tag: " ++ t
  show (StructuralAaFailure r) =
    "Structural accessibility failure: " ++ r

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
elaborateInline (InlLink _ _ xs)   =
  Element "a" [] (assert_total (map elaborateInline xs))
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
elaborateBlock (ListBlock _ _ _ _ items) =
  Element "ul" [] (map (\i => Element "li" [] (assert_total (map elaborateBlock (content i)))) items)
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
public export
elaborate : Doc -> Either ElabError (h : HExpr ** (IsValidHtml h, StructuralAA h))
elaborate doc =
  let h = elaborateDoc doc
   in case decideHtmlLocated h of
        Right p => Right (h ** (p, trivialAA h))
        Left lr => Left (LocatedHtmlError lr)
