||| HTML element catalog — Phase 2 per `plan.dj` §Phase 2 (P2.1).
|||
||| One record per element: tag name, void / raw-text flags, the content
||| categories the element belongs to, its permitted-content policy,
||| and the locally-permitted attribute set. Global attributes are
||| listed separately and apply to every element.
|||
||| `plan.dj` calls for the catalog to be ingested from machine-readable
||| upstreams (`@webref/elements`, the WHATWG HTML model). The companion
||| ingestion script is `ingest/html-model.ts`; this file is the *seed*
||| dataset that the ingestion script will round-trip against. The data
||| is therefore structured (not hand-stitched into the validator); the
||| validator interprets the data generically.
|||
||| Coverage: ~95 HTML5 elements covering everything currently emitted by
||| elaboration + TEAWeb.Html + the structural parents whose content
||| models are commonly misused (`ul`, `ol`, `dl`, `table`, `select`,
||| `picture`, `figure`, `details`). Adding a missing element is a one-
||| line edit to the `elements` list.
module Cribrum.Html.Model

import Data.List
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

--------------------------------------------------------------------------------
-- Global attributes (apply to every element).
--------------------------------------------------------------------------------

||| HTML global attributes per WHATWG §3.2.6. Element-specific catalogues
||| (`localAttrs`) extend this set.
|||
||| Includes the `aria-*` and `data-*` namespaces by literal prefix; the
||| `isAllowedAttrName` decision below handles the prefix logic.
public export
globalAttrs : List String
globalAttrs =
  [ "accesskey", "autocapitalize", "autofocus", "class"
  , "contenteditable", "dir", "draggable", "enterkeyhint"
  , "hidden", "id", "inert", "inputmode", "is", "itemid"
  , "itemprop", "itemref", "itemscope", "itemtype", "lang"
  , "nonce", "popover", "role", "slot", "spellcheck", "style"
  , "tabindex", "title", "translate"
  ]

--------------------------------------------------------------------------------
-- Element catalog.
--
-- Organised by category for readability; the validator consumes the
-- flat `elements` list below regardless of order.
--------------------------------------------------------------------------------

-- Document scaffolding -------------------------------------------------------

specHtml : ElementSpec
specHtml = MkElementSpec "html" False False [] (OnlyTags ["head", "body"] False) ["xmlns"]

specHead : ElementSpec
specHead = MkElementSpec "head" False False []
  (OnlyTags ["base", "link", "meta", "noscript", "script", "style"
            , "template", "title"] False)
  []

specBody : ElementSpec
specBody = MkElementSpec "body" False False [Flow] (OnlyCategories [Flow])
  [ "onafterprint", "onbeforeprint", "onbeforeunload", "onhashchange"
  , "onlanguagechange", "onmessage", "onmessageerror", "onoffline"
  , "ononline", "onpagehide", "onpageshow", "onpopstate"
  , "onrejectionhandled", "onstorage", "onunhandledrejection", "onunload"
  ]

specTitle : ElementSpec
specTitle = MkElementSpec "title" False False [Metadata] TextOnly []

-- Metadata-in-head ------------------------------------------------------------

specMeta : ElementSpec
specMeta = MkElementSpec "meta" True False [Metadata] NoChildren
  ["name", "http-equiv", "content", "charset", "media"]

specLink : ElementSpec
specLink = MkElementSpec "link" True False [Metadata] NoChildren
  [ "href", "crossorigin", "rel", "media", "integrity", "hreflang"
  , "type", "referrerpolicy", "sizes", "imagesrcset", "imagesizes"
  , "as", "blocking", "color", "disabled", "fetchpriority" ]

specBase : ElementSpec
specBase = MkElementSpec "base" True False [Metadata] NoChildren
  ["href", "target"]

specStyle : ElementSpec
specStyle = MkElementSpec "style" False True [Metadata] TextOnly
  ["media", "blocking"]

specScript : ElementSpec
specScript = MkElementSpec "script" False True
  [Metadata, Flow, Phrasing, ScriptSupporting] TextOnly
  [ "src", "type", "nomodule", "async", "defer", "crossorigin"
  , "integrity", "referrerpolicy", "blocking", "fetchpriority" ]

specNoscript : ElementSpec
specNoscript = MkElementSpec "noscript" False False
  [Metadata, Flow, Phrasing] AnyContent []

specTemplate : ElementSpec
specTemplate = MkElementSpec "template" False False
  [Metadata, Flow, Phrasing, ScriptSupporting] AnyContent
  ["shadowrootmode", "shadowrootdelegatesfocus", "shadowrootclonable"]

-- Sectioning content ---------------------------------------------------------

specSection : ElementSpec
specSection = MkElementSpec "section" False False
  [Flow, Sectioning, Palpable] (OnlyCategories [Flow]) []

specArticle : ElementSpec
specArticle = MkElementSpec "article" False False
  [Flow, Sectioning, Palpable] (OnlyCategories [Flow]) []

specAside : ElementSpec
specAside = MkElementSpec "aside" False False
  [Flow, Sectioning, Palpable] (OnlyCategories [Flow]) []

specNav : ElementSpec
specNav = MkElementSpec "nav" False False
  [Flow, Sectioning, Palpable] (OnlyCategories [Flow]) []

specMain : ElementSpec
specMain = MkElementSpec "main" False False
  [Flow, Palpable] (OnlyCategories [Flow]) []

specHeader : ElementSpec
specHeader = MkElementSpec "header" False False
  [Flow, Palpable] (OnlyCategories [Flow]) []

specFooter : ElementSpec
specFooter = MkElementSpec "footer" False False
  [Flow, Palpable] (OnlyCategories [Flow]) []

specSearch : ElementSpec
specSearch = MkElementSpec "search" False False
  [Flow, Palpable] (OnlyCategories [Flow]) []

specAddress : ElementSpec
specAddress = MkElementSpec "address" False False
  [Flow, Palpable] (OnlyCategories [Flow]) []

-- Headings -------------------------------------------------------------------

specH1 : ElementSpec
specH1 = MkElementSpec "h1" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []
specH2 : ElementSpec
specH2 = MkElementSpec "h2" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []
specH3 : ElementSpec
specH3 = MkElementSpec "h3" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []
specH4 : ElementSpec
specH4 = MkElementSpec "h4" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []
specH5 : ElementSpec
specH5 = MkElementSpec "h5" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []
specH6 : ElementSpec
specH6 = MkElementSpec "h6" False False [Flow, Heading, Palpable]
  (OnlyCategories [Phrasing]) []

specHgroup : ElementSpec
specHgroup = MkElementSpec "hgroup" False False [Flow, Heading, Palpable]
  (OnlyTags ["h1", "h2", "h3", "h4", "h5", "h6", "p"] False) []

-- Grouping content -----------------------------------------------------------

specP : ElementSpec
specP = MkElementSpec "p" False False [Flow, Palpable]
  (OnlyCategories [Phrasing]) []

specPre : ElementSpec
specPre = MkElementSpec "pre" False False [Flow, Palpable]
  (OnlyCategories [Phrasing]) []

specBlockquote : ElementSpec
specBlockquote = MkElementSpec "blockquote" False False [Flow, Palpable]
  (OnlyCategories [Flow]) ["cite"]

specHr : ElementSpec
specHr = MkElementSpec "hr" True False [Flow] NoChildren []

specDiv : ElementSpec
specDiv = MkElementSpec "div" False False [Flow, Palpable]
  (OnlyCategories [Flow]) []

specFigure : ElementSpec
specFigure = MkElementSpec "figure" False False [Flow, Palpable]
  -- Spec: optional figcaption + flow. We permit either by allowing both.
  AnyContent []

specFigcaption : ElementSpec
specFigcaption = MkElementSpec "figcaption" False False []
  (OnlyCategories [Flow]) []

-- Lists ----------------------------------------------------------------------

specUl : ElementSpec
specUl = MkElementSpec "ul" False False [Flow, Palpable]
  (OnlyTags ["li", "script", "template"] False) []

specOl : ElementSpec
specOl = MkElementSpec "ol" False False [Flow, Palpable]
  (OnlyTags ["li", "script", "template"] False)
  ["reversed", "start", "type"]

specMenu : ElementSpec
specMenu = MkElementSpec "menu" False False [Flow, Palpable]
  (OnlyTags ["li", "script", "template"] False) []

specLi : ElementSpec
specLi = MkElementSpec "li" False False []
  (OnlyCategories [Flow]) ["value"]

specDl : ElementSpec
specDl = MkElementSpec "dl" False False [Flow, Palpable]
  (OnlyTags ["dt", "dd", "div", "script", "template"] False) []

specDt : ElementSpec
specDt = MkElementSpec "dt" False False []
  (OnlyCategories [Flow]) []

specDd : ElementSpec
specDd = MkElementSpec "dd" False False []
  (OnlyCategories [Flow]) []

-- Tables ---------------------------------------------------------------------

specTable : ElementSpec
specTable = MkElementSpec "table" False False [Flow, Palpable]
  (OnlyTags [ "caption", "colgroup", "thead", "tbody", "tfoot", "tr"
            , "script", "template" ] False) []

specCaption : ElementSpec
specCaption = MkElementSpec "caption" False False []
  (OnlyCategories [Flow]) []

specColgroup : ElementSpec
specColgroup = MkElementSpec "colgroup" False False []
  (OnlyTags ["col", "template"] False) ["span"]

specCol : ElementSpec
specCol = MkElementSpec "col" True False [] NoChildren ["span"]

specThead : ElementSpec
specThead = MkElementSpec "thead" False False []
  (OnlyTags ["tr", "script", "template"] False) []

specTbody : ElementSpec
specTbody = MkElementSpec "tbody" False False []
  (OnlyTags ["tr", "script", "template"] False) []

specTfoot : ElementSpec
specTfoot = MkElementSpec "tfoot" False False []
  (OnlyTags ["tr", "script", "template"] False) []

specTr : ElementSpec
specTr = MkElementSpec "tr" False False []
  (OnlyTags ["td", "th", "script", "template"] False) []

specTd : ElementSpec
specTd = MkElementSpec "td" False False []
  (OnlyCategories [Flow]) ["colspan", "rowspan", "headers"]

specTh : ElementSpec
specTh = MkElementSpec "th" False False []
  (OnlyCategories [Flow])
  ["colspan", "rowspan", "headers", "scope", "abbr"]

-- Forms ----------------------------------------------------------------------

specForm : ElementSpec
specForm = MkElementSpec "form" False False [Flow, Palpable]
  (OnlyCategories [Flow])
  [ "accept-charset", "action", "autocomplete", "enctype", "method"
  , "name", "novalidate", "rel", "target" ]

specLabel : ElementSpec
specLabel = MkElementSpec "label" False False
  [Flow, Phrasing, Interactive, Palpable, FormAssociated]
  (OnlyCategories [Phrasing]) ["for"]

specInput : ElementSpec
specInput = MkElementSpec "input" True False
  [Flow, Phrasing, Interactive, FormAssociated, Palpable] NoChildren
  [ "accept", "alt", "autocomplete", "checked", "dirname"
  , "disabled", "form", "formaction", "formenctype", "formmethod"
  , "formnovalidate", "formtarget", "height", "list", "max", "maxlength"
  , "min", "minlength", "multiple", "name", "pattern", "placeholder"
  , "readonly", "required", "size", "src", "step", "type", "value"
  , "width" ]

specButton : ElementSpec
specButton = MkElementSpec "button" False False
  [Flow, Phrasing, Interactive, FormAssociated, Palpable]
  (OnlyCategories [Phrasing])
  [ "disabled", "form", "formaction", "formenctype", "formmethod"
  , "formnovalidate", "formtarget", "name", "popovertarget"
  , "popovertargetaction", "type", "value" ]

specSelect : ElementSpec
specSelect = MkElementSpec "select" False False
  [Flow, Phrasing, Interactive, FormAssociated, Palpable]
  (OnlyTags ["option", "optgroup", "hr", "script", "template"] False)
  [ "autocomplete", "disabled", "form", "multiple", "name"
  , "required", "size" ]

specDatalist : ElementSpec
specDatalist = MkElementSpec "datalist" False False [Flow, Phrasing]
  -- Either phrasing OR options. Use AnyContent for simplicity.
  AnyContent []

specOptgroup : ElementSpec
specOptgroup = MkElementSpec "optgroup" False False []
  (OnlyTags ["option", "script", "template"] False)
  ["disabled", "label"]

specOption : ElementSpec
specOption = MkElementSpec "option" False False [] TextOnly
  ["disabled", "label", "selected", "value"]

specTextarea : ElementSpec
specTextarea = MkElementSpec "textarea" False True
  [Flow, Phrasing, Interactive, FormAssociated, Palpable] TextOnly
  [ "autocomplete", "cols", "dirname", "disabled", "form", "maxlength"
  , "minlength", "name", "placeholder", "readonly", "required", "rows"
  , "wrap" ]

specOutput : ElementSpec
specOutput = MkElementSpec "output" False False
  [Flow, Phrasing, FormAssociated, Palpable]
  (OnlyCategories [Phrasing]) ["for", "form", "name"]

specProgress : ElementSpec
specProgress = MkElementSpec "progress" False False
  [Flow, Phrasing, FormAssociated, Palpable]
  (OnlyCategories [Phrasing]) ["value", "max"]

specMeter : ElementSpec
specMeter = MkElementSpec "meter" False False
  [Flow, Phrasing, FormAssociated, Palpable]
  (OnlyCategories [Phrasing])
  ["value", "min", "max", "low", "high", "optimum"]

specFieldset : ElementSpec
specFieldset = MkElementSpec "fieldset" False False
  [Flow, FormAssociated, Palpable]
  -- Optional legend + flow content. AnyContent to avoid encoding sequence.
  AnyContent ["disabled", "form", "name"]

specLegend : ElementSpec
specLegend = MkElementSpec "legend" False False []
  (OnlyCategories [Phrasing, Heading]) []

-- Interactive blocks ---------------------------------------------------------

specDetails : ElementSpec
specDetails = MkElementSpec "details" False False
  [Flow, Interactive, Palpable]
  -- summary + flow. AnyContent until sequence checking lands.
  AnyContent ["open", "name"]

specSummary : ElementSpec
specSummary = MkElementSpec "summary" False False []
  (OnlyCategories [Phrasing, Heading]) []

specDialog : ElementSpec
specDialog = MkElementSpec "dialog" False False [Flow]
  (OnlyCategories [Flow]) ["open"]

-- Embedded content -----------------------------------------------------------

specImg : ElementSpec
specImg = MkElementSpec "img" True False
  [Flow, Phrasing, Embedded, Palpable] NoChildren
  [ "alt", "src", "srcset", "sizes", "crossorigin", "usemap"
  , "ismap", "width", "height", "referrerpolicy", "decoding"
  , "loading", "fetchpriority" ]

specPicture : ElementSpec
specPicture = MkElementSpec "picture" False False [Flow, Phrasing, Embedded]
  (OnlyTags ["source", "img", "script", "template"] False) []

specSource : ElementSpec
specSource = MkElementSpec "source" True False [] NoChildren
  [ "type", "media", "src", "srcset", "sizes", "width", "height" ]

specAudio : ElementSpec
specAudio = MkElementSpec "audio" False False
  [Flow, Phrasing, Embedded, Interactive] AnyContent
  [ "src", "crossorigin", "preload", "autoplay", "loop", "muted"
  , "controls" ]

specVideo : ElementSpec
specVideo = MkElementSpec "video" False False
  [Flow, Phrasing, Embedded, Interactive, Palpable] AnyContent
  [ "src", "crossorigin", "poster", "preload", "autoplay"
  , "playsinline", "loop", "muted", "controls", "width", "height" ]

specTrack : ElementSpec
specTrack = MkElementSpec "track" True False [] NoChildren
  ["kind", "src", "srclang", "label", "default"]

specIframe : ElementSpec
specIframe = MkElementSpec "iframe" False False
  [Flow, Phrasing, Embedded, Interactive, Palpable] NoChildren
  [ "src", "srcdoc", "name", "sandbox", "allow", "allowfullscreen"
  , "width", "height", "referrerpolicy", "loading" ]

specEmbed : ElementSpec
specEmbed = MkElementSpec "embed" True False
  [Flow, Phrasing, Embedded, Interactive, Palpable] NoChildren
  ["src", "type", "width", "height"]

specObject : ElementSpec
specObject = MkElementSpec "object" False False
  [Flow, Phrasing, Embedded, Palpable] AnyContent
  ["data", "type", "name", "form", "width", "height"]

specCanvas : ElementSpec
specCanvas = MkElementSpec "canvas" False False
  [Flow, Phrasing, Embedded, Palpable] AnyContent ["width", "height"]

specMap : ElementSpec
specMap = MkElementSpec "map" False False [Flow, Phrasing, Palpable]
  AnyContent ["name"]

specArea : ElementSpec
specArea = MkElementSpec "area" True False [Flow, Phrasing] NoChildren
  [ "alt", "coords", "shape", "href", "target", "download", "ping"
  , "rel", "referrerpolicy" ]

specSvg : ElementSpec
specSvg = MkElementSpec "svg" False False
  [Flow, Phrasing, Embedded, Palpable] AnyContent []

specMath : ElementSpec
specMath = MkElementSpec "math" False False
  [Flow, Phrasing, Embedded, Palpable] AnyContent []

-- Anchor (transparent) -------------------------------------------------------

specA : ElementSpec
specA = MkElementSpec "a" False False
  [Flow, Phrasing, Interactive, Palpable] AnyContent
  [ "href", "target", "download", "ping", "rel", "hreflang", "type"
  , "referrerpolicy" ]

-- Phrasing inlines -----------------------------------------------------------

specSpan : ElementSpec
specSpan = MkElementSpec "span" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specEm : ElementSpec
specEm = MkElementSpec "em" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specStrong : ElementSpec
specStrong = MkElementSpec "strong" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specMark : ElementSpec
specMark = MkElementSpec "mark" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specSmall : ElementSpec
specSmall = MkElementSpec "small" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specS : ElementSpec
specS = MkElementSpec "s" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specCite : ElementSpec
specCite = MkElementSpec "cite" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specQ : ElementSpec
specQ = MkElementSpec "q" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) ["cite"]

specDfn : ElementSpec
specDfn = MkElementSpec "dfn" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specAbbr : ElementSpec
specAbbr = MkElementSpec "abbr" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specTime : ElementSpec
specTime = MkElementSpec "time" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) ["datetime"]

specCode : ElementSpec
specCode = MkElementSpec "code" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specVar : ElementSpec
specVar = MkElementSpec "var" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specSamp : ElementSpec
specSamp = MkElementSpec "samp" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specKbd : ElementSpec
specKbd = MkElementSpec "kbd" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specSub : ElementSpec
specSub = MkElementSpec "sub" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specSup : ElementSpec
specSup = MkElementSpec "sup" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specI : ElementSpec
specI = MkElementSpec "i" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specB : ElementSpec
specB = MkElementSpec "b" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specU : ElementSpec
specU = MkElementSpec "u" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specBdi : ElementSpec
specBdi = MkElementSpec "bdi" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) []

specBdo : ElementSpec
specBdo = MkElementSpec "bdo" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) ["dir"]

specRuby : ElementSpec
specRuby = MkElementSpec "ruby" False False [Flow, Phrasing, Palpable]
  AnyContent []

specRt : ElementSpec
specRt = MkElementSpec "rt" False False []
  (OnlyCategories [Phrasing]) []

specRp : ElementSpec
specRp = MkElementSpec "rp" False False [] TextOnly []

specData : ElementSpec
specData = MkElementSpec "data" False False [Flow, Phrasing, Palpable]
  (OnlyCategories [Phrasing]) ["value"]

specIns : ElementSpec
specIns = MkElementSpec "ins" False False [Flow, Phrasing, Palpable]
  AnyContent ["cite", "datetime"]

specDel : ElementSpec
specDel = MkElementSpec "del" False False [Flow, Phrasing]
  AnyContent ["cite", "datetime"]

specSlot : ElementSpec
specSlot = MkElementSpec "slot" False False [Flow, Phrasing] AnyContent ["name"]

specBr : ElementSpec
specBr = MkElementSpec "br" True False [Flow, Phrasing] NoChildren []

specWbr : ElementSpec
specWbr = MkElementSpec "wbr" True False [Flow, Phrasing] NoChildren []

--------------------------------------------------------------------------------
-- Catalog.
--------------------------------------------------------------------------------

||| The catalog. Order is irrelevant to the validator; grouped here by
||| spec section for human review.
public export
elements : List ElementSpec
elements =
  -- Document scaffolding
  [ specHtml, specHead, specBody, specTitle
  -- Metadata-in-head
  , specMeta, specLink, specBase, specStyle, specScript, specNoscript, specTemplate
  -- Sectioning
  , specSection, specArticle, specAside, specNav
  , specMain, specHeader, specFooter, specSearch, specAddress
  -- Headings
  , specH1, specH2, specH3, specH4, specH5, specH6, specHgroup
  -- Grouping
  , specP, specPre, specBlockquote, specHr, specDiv
  , specFigure, specFigcaption
  -- Lists
  , specUl, specOl, specMenu, specLi, specDl, specDt, specDd
  -- Tables
  , specTable, specCaption, specColgroup, specCol
  , specThead, specTbody, specTfoot, specTr, specTd, specTh
  -- Forms
  , specForm, specLabel, specInput, specButton
  , specSelect, specDatalist, specOptgroup, specOption
  , specTextarea, specOutput, specProgress, specMeter
  , specFieldset, specLegend
  -- Interactive blocks
  , specDetails, specSummary, specDialog
  -- Embedded
  , specImg, specPicture, specSource, specAudio, specVideo, specTrack
  , specIframe, specEmbed, specObject, specCanvas
  , specMap, specArea, specSvg, specMath
  -- Anchor
  , specA
  -- Phrasing inlines
  , specSpan, specEm, specStrong, specMark, specSmall, specS
  , specCite, specQ, specDfn, specAbbr, specTime, specCode, specVar
  , specSamp, specKbd, specSub, specSup, specI, specB, specU
  , specBdi, specBdo, specRuby, specRt, specRp, specData
  , specIns, specDel, specSlot, specBr, specWbr
  ]

--------------------------------------------------------------------------------
-- Lookups.
--------------------------------------------------------------------------------

||| Find the spec for `tag` in the catalog. Returns `Nothing` if the tag
||| is not in the catalog (unknown elements are rejected upstream by
||| `IsKnownTag` in `Cribrum.Html.Valid`; this lookup is the catalog-side
||| half of that decision).
public export
lookupSpec : String -> Maybe ElementSpec
lookupSpec t = find (\s => name s == t) elements

||| `True` iff `tag` is in the catalog.
public export
isKnownTagBool : String -> Bool
isKnownTagBool t = case lookupSpec t of
  Just _  => True
  Nothing => False

||| Convenience: the catalog's allowed tag names.
public export
knownTagNames : List String
knownTagNames = map name elements

||| Categories assigned to `tag`. Unknown tag → empty list.
public export
categoriesOf : String -> List Category
categoriesOf t = case lookupSpec t of
  Just s  => categories s
  Nothing => []

||| Permitted-content policy for `tag`. Unknown tag → `NoChildren` (the
||| safest fallback; the unknown-tag rejection fires first).
public export
childPolicyOf : String -> ChildPolicy
childPolicyOf t = case lookupSpec t of
  Just s  => childPolicy s
  Nothing => NoChildren

||| Local attributes for `tag` (does NOT include `globalAttrs`). Unknown
||| tag → empty list.
public export
localAttrsOf : String -> List String
localAttrsOf t = case lookupSpec t of
  Just s  => localAttrs s
  Nothing => []

||| `True` iff `tag` is in HTML 5's void-element set per the catalog.
public export
isVoidTag : String -> Bool
isVoidTag t = case lookupSpec t of
  Just s  => isVoid s
  Nothing => False

--------------------------------------------------------------------------------
-- Attribute name permission.
--
-- An attribute name is allowed on a given element iff it is:
--   1. In `globalAttrs`, OR
--   2. In the element's `localAttrs`, OR
--   3. A `data-*` attribute (custom data), OR
--   4. An `aria-*` attribute (ARIA), OR
--   5. An `on*` event handler attribute (HTML allows these on most elements).
--
-- The decision is total + boolean; the type-level lift lives in
-- `Cribrum.Html.Valid`.
--------------------------------------------------------------------------------

isDataAttr : String -> Bool
isDataAttr s = case unpack s of
  ('d' :: 'a' :: 't' :: 'a' :: '-' :: _) => True
  _                                       => False

isAriaAttr : String -> Bool
isAriaAttr s = case unpack s of
  ('a' :: 'r' :: 'i' :: 'a' :: '-' :: _) => True
  _                                       => False

isOnEventAttr : String -> Bool
isOnEventAttr s = case unpack s of
  ('o' :: 'n' :: _) => True
  _                 => False

||| Decide whether `attrName` is permitted on element `tag`.
||| Total. Used by `Cribrum.Html.Valid` to build the `AttrAllowedIn`
||| witness.
public export
isAllowedAttrName : (tag : String) -> (attrName : String) -> Bool
isAllowedAttrName tag a =
     elem a globalAttrs
  || elem a (localAttrsOf tag)
  || isDataAttr a
  || isAriaAttr a
  || isOnEventAttr a
