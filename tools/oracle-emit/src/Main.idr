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
decideBool h = case decideHtml h of
  Yes _ => True
  No  _ => False

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
