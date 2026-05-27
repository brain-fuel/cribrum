||| `oracle-emit` — Cribrum side of the P2.4 cross-check oracle.
|||
||| Per `plan.dj` §P2.4: "PBT corpus. Known-good drawn from MDN / spec
||| examples + mutation-shrunk known-bad. Oracle cross-check against
||| an FFI'd reference validator (`validator.js`, vnu-jar) used only
||| to check, never on the runtime path."
|||
||| This tool ships the *Cribrum side* of that oracle: a curated
||| corpus of `(name, HExpr, expectedValid)` triples that exercise the
||| Phase-2 `IsValidHtml` decision in both directions (valid + invalid)
||| across the major content-model rejection classes. For each case
||| the tool emits a JSONL row:
|||
|||     {"name":"…", "decided":<bool>, "expected":<bool>, "html":"…"}
|||
||| `decided`  — Cribrum's `decideHtml` verdict (`Yes` / `No`).
||| `expected` — the curated ground truth: what HTML5 actually says.
||| `html`     — Cribrum's `renderHtml` output for the HExpr.
|||
||| The companion Node tool `ingest/oracle.ts` consumes this JSONL,
||| feeds each `html` (wrapped in a minimal HTML5 doc shell) through
||| the W3C `vnu.jar` validator, and flags disagreements: any row
||| where Cribrum, the curated expectation, and vnu do not all agree
||| (modulo features Cribrum doesn't yet model) is a finding for
||| follow-up.
|||
||| Internal consistency: if `decided` ≠ `expected` for any row, the
||| tool exits non-zero — that's a Cribrum-side disagreement against
||| its own corpus, independent of vnu. Catching it here means the
||| Node-side oracle only ever sees rows Cribrum and the corpus agree
||| on; any disagreement vnu surfaces is a real ambiguity.
module Main

import System
import System.File
import Data.List
import Data.String
import Cribrum.Node
import Cribrum.Html.Valid
import Cribrum.Render.Html

%default total

--------------------------------------------------------------------------------
-- Corpus.
--------------------------------------------------------------------------------

record Case where
  constructor MkCase
  name     : String
  expected : Bool   -- True = expected valid by HTML5
  expr     : HExpr

||| Curated corpus. Each row exercises one Phase-2 decision class.
|||
||| Valid examples (`expected = True`) draw from common HTML5 idioms
||| documented on MDN / WHATWG: paragraph with phrasing, sectioning
||| landmarks with nested headings, list/li, table with thead/tbody,
||| anchor with href, image with alt, figure/figcaption.
|||
||| Invalid examples (`expected = False`) target every located-
||| rejection class in `Cribrum.Html.Valid`: UnknownTag,
||| DisallowedAttr, IllegalChild (block-in-phrasing, non-li in ul),
||| MalformedTable, TextNotAllowedIn, CommentNotAllowedIn. Each is the
||| minimal counterexample: one offending node inside an otherwise
||| valid wrapper, so the rejection is unambiguous.
corpus : List Case
corpus =
  [ MkCase "valid/paragraph" True $
      Element "p" [] [Text "hello world"]
  , MkCase "valid/sectioning-nested-headings" True $
      Element "section" []
        [ Element "h1" [] [Text "Section"]
        , Element "p" [] [Text "body"]
        ]
  , MkCase "valid/ul-li" True $
      Element "ul" []
        [ Element "li" [] [Text "one"]
        , Element "li" [] [Text "two"]
        ]
  , MkCase "valid/table-thead-tbody" True $
      Element "table" []
        [ Element "thead" []
            [ Element "tr" []
                [ Element "th" [] [Text "h"] ]
            ]
        , Element "tbody" []
            [ Element "tr" []
                [ Element "td" [] [Text "v"] ]
            ]
        ]
  , MkCase "valid/anchor-href" True $
      Element "a" [MkHAttr "href" (Str "/x")] [Text "link"]
  , MkCase "valid/img-alt-src" True $
      Element "img"
        [ MkHAttr "src" (Str "/x.png")
        , MkHAttr "alt" (Str "alt text")
        ] []
  , MkCase "valid/figure-figcaption" True $
      Element "figure" []
        [ Element "img"
            [ MkHAttr "src" (Str "/x.png")
            , MkHAttr "alt" (Str "alt")
            ] []
        , Element "figcaption" [] [Text "caption"]
        ]

  -- UnknownTag.
  , MkCase "invalid/unknown-tag" False $
      Element "frobnicator" [] []

  -- DisallowedAttr — `href` belongs on `a`/`area`/`link`, not on `p`.
  , MkCase "invalid/disallowed-attr-href-on-p" False $
      Element "p" [MkHAttr "href" (Str "/x")] [Text "no"]

  -- IllegalChild — `<p>` accepts phrasing only; `<div>` is flow.
  , MkCase "invalid/block-in-phrasing" False $
      Element "p" []
        [ Element "div" [] [Text "block inside phrasing"] ]

  -- IllegalChild — `<ul>` admits only `<li>` (+ `<script>` / `<template>`).
  , MkCase "invalid/non-li-in-ul" False $
      Element "ul" []
        [ Element "p" [] [Text "not an li"] ]

  -- MalformedTable — `<td>` must live inside a `<tr>`, never as a
  -- direct child of `<table>`. (`<tr>` directly inside `<table>` is
  -- permitted by HTML5 for backward compatibility, so the canonical
  -- malformed-table counterexample is a stray `<td>`.)
  , MkCase "invalid/table-td-as-direct-child" False $
      Element "table" []
        [ Element "td" [] [Text "v"] ]

  -- ============================================================
  -- Expanded corpus (P2.4): edge cases across nested phrasing/flow
  -- boundaries, void elements, form controls, ARIA, sectioning
  -- landmarks, and known-bad classes still under the existing
  -- validator's coverage.
  -- ============================================================

  -- Valid: void elements appear as childless leaves.
  , MkCase "valid/void-br-in-paragraph" True $
      Element "p" []
        [ Text "line one"
        , Element "br" [] []
        , Text "line two"
        ]
  , MkCase "valid/void-hr-flow" True $
      Element "section" []
        [ Element "h1" [] [Text "title"]
        , Element "hr" [] []
        , Element "p" [] [Text "after rule"]
        ]
  , MkCase "valid/void-wbr-in-paragraph" True $
      Element "p" []
        [ Text "longword"
        , Element "wbr" [] []
        , Text "continues"
        ]

  -- Valid: form controls under fieldset/legend, label/for-id pairing
  -- via ID, button/text submission.
  , MkCase "valid/form-controls-with-fieldset" True $
      Element "form" []
        [ Element "fieldset" []
            [ Element "legend" [] [Text "Profile"]
            , Element "label"
                [MkHAttr "for" (Str "n")] [Text "Name"]
            , Element "input"
                [ MkHAttr "id" (Str "n")
                , MkHAttr "type" (Str "text")
                ] []
            , Element "button"
                [MkHAttr "type" (Str "submit")] [Text "Save"]
            ]
        ]

  -- Valid: sectioning landmarks (header/nav/main/aside/footer).
  -- Wrapped in a div, not a `<body>`, because the oracle's vnu-side
  -- wrap already supplies a `<body>` and nested body is invalid.
  , MkCase "valid/page-landmarks" True $
      Element "div" []
        [ Element "header" []
            [Element "h1" [] [Text "Site"]]
        , Element "nav" []
            [Element "a" [MkHAttr "href" (Str "/x")] [Text "x"]]
        , Element "main" []
            [Element "article" []
              [Element "h1" [] [Text "Post"]]]
        , Element "aside" [] [Text "side"]
        , Element "footer" [] [Text "© 2026"]
        ]

  -- Valid: details/summary disclosure widget.
  , MkCase "valid/details-summary" True $
      Element "details" []
        [ Element "summary" [] [Text "More"]
        , Element "p" [] [Text "Hidden body."]
        ]

  -- Valid: dl/dt/dd term-and-definition pairing.
  , MkCase "valid/dl-dt-dd" True $
      Element "dl" []
        [ Element "dt" [] [Text "Term"]
        , Element "dd" [] [Text "Definition body."]
        ]

  -- Valid: ARIA attributes on interactive elements (button + label).
  , MkCase "valid/aria-label-on-button" True $
      Element "button"
        [ MkHAttr "type" (Str "button")
        , MkHAttr "aria-label" (Str "Close dialog")
        ] [Text "×"]

  -- Valid: ARIA describedby pointing at sibling text.
  , MkCase "valid/aria-describedby" True $
      Element "div" []
        [ Element "input"
            [ MkHAttr "id" (Str "u")
            , MkHAttr "type" (Str "text")
            , MkHAttr "aria-describedby" (Str "help")
            ] []
        , Element "p"
            [MkHAttr "id" (Str "help")]
            [Text "Letters only."]
        ]

  -- Valid: figure with picture + sources.
  , MkCase "valid/picture-source-img" True $
      Element "picture" []
        [ Element "source"
            [ MkHAttr "srcset" (Str "/x.webp")
            , MkHAttr "type"   (Str "image/webp")
            ] []
        , Element "img"
            [ MkHAttr "src" (Str "/x.png")
            , MkHAttr "alt" (Str "x")
            ] []
        ]

  -- Valid: time element with datetime.
  , MkCase "valid/time-with-datetime" True $
      Element "p" []
        [ Text "Posted "
        , Element "time"
            [MkHAttr "datetime" (Str "2026-05-26")]
            [Text "May 26"]
        ]

  -- Valid: pre/code language-styled.
  , MkCase "valid/pre-code-with-lang" True $
      Element "pre" []
        [ Element "code"
            [MkHAttr "class" (Str "language-idris")]
            [Text "main : IO ()"]
        ]

  -- Valid: progress + meter.
  , MkCase "valid/progress-meter" True $
      Element "div" []
        [ Element "progress"
            [ MkHAttr "value" (Str "0.5")
            , MkHAttr "max"   (Str "1")
            ] [Text "50%"]
        , Element "meter"
            [ MkHAttr "value" (Str "3")
            , MkHAttr "min"   (Str "0")
            , MkHAttr "max"   (Str "10")
            ] [Text "3 of 10"]
        ]

  -- Valid: blockquote with cite attribute.
  , MkCase "valid/blockquote-with-cite" True $
      Element "blockquote"
        [MkHAttr "cite" (Str "https://example.org/src")]
        [Element "p" [] [Text "Quoted text."]]

  -- Valid: nested unordered lists (ul > li > ul > li).
  , MkCase "valid/nested-ul" True $
      Element "ul" []
        [ Element "li" []
            [ Text "outer"
            , Element "ul" []
                [Element "li" [] [Text "inner"]]
            ]
        ]

  -- Valid: img with explicit dimensions + loading hint.
  , MkCase "valid/img-with-dims-and-loading" True $
      Element "img"
        [ MkHAttr "src"    (Str "/x.png")
        , MkHAttr "alt"    (Str "x")
        , MkHAttr "width"  (Str "640")
        , MkHAttr "height" (Str "480")
        , MkHAttr "loading" (Str "lazy")
        ] []

  -- Valid: mark element inside flow phrasing.
  , MkCase "valid/mark-in-paragraph" True $
      Element "p" []
        [ Text "find the "
        , Element "mark" [] [Text "highlighted"]
        , Text " word"
        ]

  -- Valid: input types beyond text — checkbox + radio under fieldset.
  , MkCase "valid/checkbox-radio-fieldset" True $
      Element "fieldset" []
        [ Element "legend" [] [Text "Choices"]
        , Element "input"
            [ MkHAttr "id"   (Str "c1")
            , MkHAttr "type" (Str "checkbox")
            ] []
        , Element "label" [MkHAttr "for" (Str "c1")] [Text "Yes"]
        , Element "input"
            [ MkHAttr "id"   (Str "r1")
            , MkHAttr "type" (Str "radio")
            , MkHAttr "name" (Str "g")
            ] []
        , Element "label" [MkHAttr "for" (Str "r1")] [Text "One"]
        ]

  -- (Removed: invalid/void-br-with-children + invalid/void-img-with-children.
  -- Cribrum's renderer drops the children of void elements before
  -- serialisation, so the HTML reaching vnu is `<br>` / `<img …>` —
  -- both of which vnu rightly accepts. The HExpr-side violation
  -- exists, but the rendered string carries no trace of it, leaving
  -- nothing for the vnu cross-check to disagree with. Cribrum's own
  -- `decideHtml` already catches this class via `ChildPolicy.None`
  -- on void specs; oracle row would be redundant.)

  -- Invalid: flow-only child inside phrasing — `<p>` cannot contain
  -- `<section>`.
  , MkCase "invalid/section-in-paragraph" False $
      Element "p" []
        [ Element "section" [] [Text "no"] ]

  -- Invalid: `<select>` admits only `<option>` / `<optgroup>` /
  -- `<hr>`. A `<p>` here is an illegal child.
  , MkCase "invalid/p-in-select" False $
      Element "select" []
        [ Element "p" [] [Text "no"] ]

  -- Invalid: text directly in `<table>` (text not allowed in
  -- whitespace-strict parents).
  , MkCase "invalid/text-in-table" False $
      Element "table" [] [Text "stray text"]

  -- ============================================================
  -- Ancestor-context rejections — re-introduced once the Phase-2
  -- ancestor pass in `Cribrum.Html.Valid.locateAncestor` landed
  -- (rejection classes `InteractiveInInteractive`, `FormInForm`,
  -- `CommentInRawText`, `OrphanLi`, `OrphanDtDd`).
  -- ============================================================

  -- Invalid: interactive-in-interactive. `<a>` content model forbids
  -- interactive content descendants — including `<button>`.
  , MkCase "invalid/interactive-in-anchor" False $
      Element "a" [MkHAttr "href" (Str "/x")]
        [Element "button" [MkHAttr "type" (Str "button")] [Text "x"]]

  -- Invalid: anchor-in-anchor (a strict-er case of interactive-in-
  -- interactive — `<a>` also forbids `<a>` descendants).
  , MkCase "invalid/anchor-in-anchor" False $
      Element "a" [MkHAttr "href" (Str "/outer")]
        [Element "a" [MkHAttr "href" (Str "/inner")] [Text "x"]]

  -- Invalid: interactive descendant of `<button>` — `<button>` forbids
  -- interactive content descendants per HTML5.
  , MkCase "invalid/interactive-in-button" False $
      Element "button" [MkHAttr "type" (Str "button")]
        [Element "input" [MkHAttr "type" (Str "text")] []]

  -- Invalid: nested forms — `<form>` content model is "flow content
  -- but with no form element descendants".
  , MkCase "invalid/form-in-form" False $
      Element "form" []
        [Element "form" []
          [Element "input" [MkHAttr "type" (Str "text")] []]]

  -- Invalid: comment inside `<style>` — raw-text content model
  -- admits text only; `<!--` is character data, not an HTML comment.
  -- (The corresponding `<script>` variant was elided from this corpus:
  -- vnu accepts `<script><!-- … --></script>` because the browser
  -- model parses `<!-- … -->` as script source, masking the IR-level
  -- violation. Cribrum still rejects it via `CommentInRawText`; the
  -- gap is internal-only, not vnu-cross-checkable.)
  , MkCase "invalid/comment-in-style" False $
      Element "style" [] [Comment "/* x */"]

  -- Invalid: orphan `<li>` under an AnyContent parent (`<ins>`).
  -- Structural parents like `<div>` already reject `<li>` via
  -- IllegalChild (li's categories are empty, so the structural
  -- locator fires first); under AnyContent the ancestor pass is the
  -- only line of defence.
  , MkCase "invalid/orphan-li-under-ins" False $
      Element "ins" []
        [Element "li" [] [Text "stray"]]

  -- Invalid: orphan `<dd>` under an AnyContent parent (`<ins>`).
  , MkCase "invalid/orphan-dd-under-ins" False $
      Element "ins" []
        [Element "dd" [] [Text "definition"]]
  ]

--------------------------------------------------------------------------------
-- JSON escaping + emission.
--------------------------------------------------------------------------------

||| Escape a string for emission inside a JSON double-quoted literal.
||| Handles the minimum set the corpus needs: backslash, double-quote,
||| newline, carriage return, tab. Other control characters fall
||| through verbatim (the corpus is ASCII-only by construction).
jsonEscape : String -> String
jsonEscape = pack . concatMap esc . unpack
  where
    esc : Char -> List Char
    esc '\\' = ['\\', '\\']
    esc '"'  = ['\\', '"']
    esc '\n' = ['\\', 'n']
    esc '\r' = ['\\', 'r']
    esc '\t' = ['\\', 't']
    esc c    = [c]

||| Emit one JSONL row for a case + verdict pair.
emitRow : Case -> (decided : Bool) -> (html : String) -> String
emitRow c d h =
  "{\"name\":\""    ++ jsonEscape (name c)     ++ "\","
  ++ "\"expected\":" ++ (if expected c then "true" else "false") ++ ","
  ++ "\"decided\":"  ++ (if d          then "true" else "false") ++ ","
  ++ "\"html\":\""   ++ jsonEscape h            ++ "\"}"

--------------------------------------------------------------------------------
-- Main.
--------------------------------------------------------------------------------

decideBool : HExpr -> Bool
decideBool = isValidHtmlLocated

||| Run one case: emit JSONL + report disagreement with curated
||| expected verdict. Returns `True` iff Cribrum agrees with the
||| curated expectation.
covering
runCase : Case -> IO Bool
runCase c = do
  let decided = decideBool (expr c)
  let html = renderHtml (expr c)
  let row = emitRow c decided html
  putStrLn row
  pure (decided == expected c)

covering
runCases : List Case -> IO Nat
runCases [] = pure 0
runCases (c :: cs) = do
  ok <- runCase c
  rest <- runCases cs
  pure (if ok then rest else S rest)

covering
main : IO ()
main = do
  disagreed <- runCases corpus
  Right () <- fPutStrLn stderr
                ("oracle-emit: " ++ show (length corpus) ++ " case(s), "
                  ++ show disagreed ++ " Cribrum/corpus disagreement(s)")
    | Left _ => exitFailure
  if disagreed == Z
    then exitSuccess
    else exitFailure
