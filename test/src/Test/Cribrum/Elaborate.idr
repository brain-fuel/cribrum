module Test.Cribrum.Elaborate

import Data.Vect
import Data.String
import Hedgehog
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser
import Cribrum.Html.Valid
import Cribrum.Elaborate
import Cribrum.Render.Html

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- EXTs.
--------------------------------------------------------------------------------

para : String -> Block
para s = Paragraph emptyAttrs [InlText s]

heading : Nat -> String -> Block
heading n s = Heading emptyAttrs n [InlText s]

doc : List Block -> Doc
doc = MkDoc

||| A fenced `:::cls` div carrying only classes (no id / pairs).
divc : List String -> List Block -> Block
divc cs = Div (MkAttrs Nothing cs [])

||| An inline `[..]{.cls}` span carrying only classes (no id / pairs).
spanc : List String -> List Inline -> Inline
spanc cs = InlSpan (MkAttrs Nothing cs [])

export
ext_empty_doc_elaborates_to_empty_main : Property
ext_empty_doc_elaborates_to_empty_main = oneShot $
  elaborateDoc (doc []) === Element "main" [] []

export
ext_paragraph_becomes_p : Property
ext_paragraph_becomes_p = oneShot $
  elaborateDoc (doc [para "hi"])
    === Element "main" [] [Element "p" [] [Text "hi"]]

||| A heading at the document top level is wrapped in a `<section>`
||| (id-less until `Cribrum.Pipeline.Anchor.addSectionIds` runs); the
||| heading tag still maps from the level.
export
ext_heading_levels_map_to_h_tags : Property
ext_heading_levels_map_to_h_tags = oneShot $
  elaborateDoc (doc [heading 3 "T"])
    === Element "main" [] [Element "section" [] [Element "h3" [] [Text "T"]]]

||| Two same-level headings produce two SIBLING sections, not one nested
||| inside the other — a heading of level <= the open section's level
||| closes it. (Mutant-kill on `closesSection`'s `<=`.)
export
ext_sibling_headings_are_sibling_sections : Property
ext_sibling_headings_are_sibling_sections = oneShot $
  elaborateDoc (doc [heading 1 "A", heading 1 "B"])
    === Element "main" []
          [ Element "section" [] [Element "h1" [] [Text "A"]]
          , Element "section" [] [Element "h1" [] [Text "B"]]
          ]

||| A deeper heading nests as a CHILD section of the shallower one.
export
ext_deeper_heading_nests : Property
ext_deeper_heading_nests = oneShot $
  elaborateDoc (doc [heading 1 "A", heading 2 "B"])
    === Element "main" []
          [ Element "section" []
              [ Element "h1" [] [Text "A"]
              , Element "section" [] [Element "h2" [] [Text "B"]]
              ]
          ]

export
ext_thematic_becomes_hr : Property
ext_thematic_becomes_hr = oneShot $
  elaborateDoc (doc [ThematicBreak emptyAttrs])
    === Element "main" [] [Element "hr" [] []]

export
ext_softbreak_becomes_space : Property
ext_softbreak_becomes_space = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs
           [InlText "a", InlSoftBreak, InlText "b"]])
    === Element "main" []
          [Element "p" [] [Text "a", Text " ", Text "b"]]

export
ext_hardbreak_becomes_br : Property
ext_hardbreak_becomes_br = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlText "a", InlHardBreak, InlText "b"]])
    === Element "main" []
          [Element "p" [] [Text "a", Element "br" [] [], Text "b"]]

export
ext_emphasis_becomes_em : Property
ext_emphasis_becomes_em = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlEmph [InlText "x"]]])
    === Element "main" []
          [Element "p" [] [Element "em" [] [Text "x"]]]

export
ext_strong_becomes_strong : Property
ext_strong_becomes_strong = oneShot $
  elaborateDoc
    (doc [Paragraph emptyAttrs [InlStrong [InlText "x"]]])
    === Element "main" []
          [Element "p" [] [Element "strong" [] [Text "x"]]]

||| Strict elaboration: the produced HExpr passes IsValidHtml; the
||| Either-Right carries the witness.
export
ext_strict_elaboration_carries_proof : Property
ext_strict_elaboration_carries_proof = oneShot $
  case elaborate (doc [para "hi", heading 2 "T"]) of
    Right _ => success
    Left  e => failWith Nothing ("expected Right; got " ++ show e)

||| Round trip: parsed input -> elaborated HExpr is structurally valid HTML.
export
ext_round_trip_parse_then_elaborate : Property
ext_round_trip_parse_then_elaborate = oneShot $
  case parseDoc "# Title\n\nA paragraph." of
    Right d => case elaborate d of
      Right _ => success
      Left  e => failWith Nothing ("elaborate failed: " ++ show e)
    Left e  => failWith Nothing ("parse failed: " ++ show e)

||| Strict elaboration of a skipped heading sequence (h1 -> h3) is rejected
||| with the `heading-no-skip` rule id surfaced through `StructuralAaFailure`.
||| heading-no-skip is a whole-tree rule, so the path is `Nothing`. Pins the
||| Phase-4 codomain wiring: a structurally-AA failure short-circuits through
||| `decStructuralAA` and is reported with its catalog id.
export
ext_strict_elaboration_rejects_skipped_headings : Property
ext_strict_elaboration_rejects_skipped_headings = oneShot $
  case elaborate (doc [heading 1 "A", heading 3 "C"]) of
    Right _                            =>
      failWith Nothing "expected StructuralAaFailure heading-no-skip"
    Left (StructuralAaFailure "heading-no-skip" Nothing) => success
    Left e =>
      failWith Nothing ("wrong error variant: " ++ show e)

||| Per-node-rule failure (`button-name`) surfaces with path-into-tree.
||| Hand-builds an HExpr the parser slice can't yet emit: `<main>` containing
||| a `<button>` with no accessible name. The button is at child index 0 of
||| the `<main>` root, so the located path is `Just [0]`. Pins the Phase-4
||| §P4.3 "each hard error is located" contract for per-node rules.
export
ext_per_node_rule_failure_is_located : Property
ext_per_node_rule_failure_is_located = oneShot $
  let bad : HExpr
      bad = Element "main" [] [Element "button" [] []]
   in case decStructuralAA bad of
        Right _                                            =>
          failWith Nothing "expected button-name rejection"
        Left ("button-name", Just [0]) => success
        Left other                                         =>
          failWith Nothing ("wrong rule/path: " ++ show other)

||| Whole-tree rule `unique-main` surfaces with `path = Nothing` (the
||| violation is the relation between multiple nodes, not any single
||| node). Hand-builds a tree with two sibling `<main>` elements.
export
ext_unique_main_failure_whole_tree : Property
ext_unique_main_failure_whole_tree = oneShot $
  let bad : HExpr
      bad = Element "div" []
              [ Element "main" [] []
              , Element "main" [] []
              ]
   in case decStructuralAA bad of
        Right _                                  =>
          failWith Nothing "expected unique-main rejection"
        Left ("unique-main", Nothing) => success
        Left other                               =>
          failWith Nothing ("wrong rule/path: " ++ show other)

--------------------------------------------------------------------------------
-- PDDTs.
--------------------------------------------------------------------------------

||| Heading level -> tag name (1..6).
headingTagCases : List (Nat, String)
headingTagCases =
  [ (1, "h1"), (2, "h2"), (3, "h3"), (4, "h4"), (5, "h5"), (6, "h6") ]

export
pddt_heading_tags : Property
pddt_heading_tags = withTests 1 . property $ do
  for_ headingTagCases $ \(lvl, tag) =>
    elaborateBlock (heading lvl "T")
      === Element tag [] [Text "T"]

||| Inline-construct -> tag mapping; pin the catalog.
inlineMappingCases : List (Inline, HExpr)
inlineMappingCases =
  [ (InlText "x",                       Text "x")
  , (InlSoftBreak,                      Text " ")
  , (InlHardBreak,                      Element "br" [] [])
  , (InlEmph    [InlText "x"],          Element "em"     [] [Text "x"])
  , (InlStrong  [InlText "x"],          Element "strong" [] [Text "x"])
  , (InlHighlight [InlText "x"],        Element "mark"   [] [Text "x"])
  , (InlSuper   [InlText "x"],          Element "sup"    [] [Text "x"])
  , (InlSub     [InlText "x"],          Element "sub"    [] [Text "x"])
  , (InlInsert  [InlText "x"],          Element "ins"    [] [Text "x"])
  , (InlDelete  [InlText "x"],          Element "del"    [] [Text "x"])
  , (InlVerbatim emptyAttrs "code",     Element "code"   [] [Text "code"])
  , ( InlMath False "a^2"
    , Element "span" [MkHAttr "class" (Str "math inline")] [Text "\\(a^2\\)"]
    )
  , ( InlMath True "a^2"
    , Element "span" [MkHAttr "class" (Str "math display")] [Text "\\[a^2\\]"]
    )
  , (InlSpan emptyAttrs [InlText "x"],  Element "span"   [] [Text "x"])
  , (InlSmart EmDash,                   Text "\x2014")
  , (InlSmart Ellipsis,                 Text "\x2026")
  , ( InlImage emptyAttrs (LinkInline "/x.png" Nothing) [InlText "a cat"]
    , Element "img" [ MkHAttr "alt" (Str "a cat")
                    , MkHAttr "src" (Str "/x.png")
                    ] []
    )
  , ( InlImage emptyAttrs (LinkInline "/dec.png" Nothing) []
    , Element "img" [ MkHAttr "alt" (Str "")
                    , MkHAttr "src" (Str "/dec.png")
                    ] []
    )
  , ( InlLink emptyAttrs (LinkAuto "https://example.org")
        [InlText "https://example.org"]
    , Element "a" [MkHAttr "href" (Str "https://example.org")]
        [Text "https://example.org"]
    )
  ]

||| Inline math wraps the verbatim body in `\(…\)` inside
||| `<span class="math inline">`. Pins both the class and the delimiters.
export
ext_inline_math_span : Property
ext_inline_math_span = oneShot $
  elaborateInline (InlMath False "e=mc^2")
    === Element "span" [MkHAttr "class" (Str "math inline")]
          [Text "\\(e=mc^2\\)"]

||| Display math (`$$`) uses class `math display` and `\[…\]` delimiters.
export
ext_display_math_span : Property
ext_display_math_span = oneShot $
  elaborateInline (InlMath True "e=mc^2")
    === Element "span" [MkHAttr "class" (Str "math display")]
          [Text "\\[e=mc^2\\]"]

||| Image-with-emphasis-in-alt: nested inlines flatten to the alt text.
||| Catches regressions where `inlinesPlainText` drops a constructor.
export
ext_image_alt_flattens_emphasis : Property
ext_image_alt_flattens_emphasis = oneShot $
  elaborateInline
    (InlImage emptyAttrs (LinkInline "/x.png" Nothing)
       [InlText "see ", InlEmph [InlText "this"], InlText "!"])
    === Element "img"
          [ MkHAttr "alt" (Str "see this!")
          , MkHAttr "src" (Str "/x.png")
          ] []

||| A resolved link carrying a title emits `<a href title>`. Pins the
||| `titleHAttr` emission for the link side.
export
ext_link_title_emitted : Property
ext_link_title_emitted = oneShot $
  elaborateInline
    (InlLink emptyAttrs (LinkInline "/url" (Just "foo")) [InlText "ref"])
    === Element "a"
          [ MkHAttr "href" (Str "/url")
          , MkHAttr "title" (Str "foo")
          ] [Text "ref"]

||| A link's OWN inline `{title=…}` attribute overrides the reference
||| definition's title (djot links-and-images-014). The link carries
||| `{title=bar}` while the resolved destination carries title `foo`;
||| the author's inline attr wins. Pins the title-precedence in the
||| `InlLink` elaborator.
export
ext_link_inline_title_overrides_ref : Property
ext_link_inline_title_overrides_ref = oneShot $
  elaborateInline
    (InlLink (MkAttrs Nothing [] [("title", "bar")])
             (LinkInline "/url" (Just "foo")) [InlText "ref"])
    === Element "a"
          [ MkHAttr "href" (Str "/url")
          , MkHAttr "title" (Str "bar")
          ] [Text "ref"]

||| Strict `elaborate` fills an empty-destination anchor's href with
||| `#` (bucket C) so the document renders `<a href="#">` instead of
||| tripping the `link-empty-href` gate. Pins the `fillHrefs` strict
||| default. (`elaborateInline` alone still emits `href=""`; the fill
||| happens in the strict pass so draft mode keeps its repair+warn.)
export
ext_link_empty_href_defaults_to_hash : Property
ext_link_empty_href_defaults_to_hash = oneShot $
  case elaborate (MkDoc [Paragraph emptyAttrs
                          [InlLink emptyAttrs (LinkInline "" Nothing)
                             [InlText "link"]]]) of
    Left  e            => failWith Nothing ("elaborate failed: " ++ show e)
    Right (h ** (_, _)) =>
      isInfixOf "<a href=\"#\">link</a>" (renderHtml h) === True

||| A link carrying a valid global author attribute (`{lang=fr}`) rides
||| it through onto the `<a>` after the mandatory `href`. Pins that the
||| link elaborator now threads its `Attrs` (previously dropped).
export
ext_link_carries_valid_author_attr : Property
ext_link_carries_valid_author_attr = oneShot $
  elaborateInline
    (InlLink (MkAttrs Nothing [] [("lang", "fr")])
             (LinkInline "/url" Nothing) [InlText "x"])
    === Element "a"
          [ MkHAttr "href" (Str "/url")
          , MkHAttr "lang" (Str "fr")
          ] [Text "x"]

||| End-to-end: an attribute block preceding a reference definition
||| (`{title=foo}\n[ref]: /url`) flows the title onto a resolved
||| `[ref][]` reference link (djot links-and-images-013). Pins the
||| parser refdef-attr plumbing + elaborator title emission together.
export
ext_refdef_attr_title_flows_to_link : Property
ext_refdef_attr_title_flows_to_link = oneShot $
  case parseDoc "{title=foo}\n[ref]: /url\n\n[ref][]\n" of
    Left  e => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left  e            => failWith Nothing ("elaborate failed: " ++ show e)
      Right (h ** (_, _)) =>
        let out = renderHtml h
         in if isInfixOf "<a href=\"/url\" title=\"foo\">ref</a>" out
              then success
              else failWith Nothing ("unexpected render: " ++ out)

||| End-to-end: a NON-title attribute on a reference definition
||| (`{lang=fr}\n[ref]: /url`) now flows onto the resolved `[ref][]`
||| link alongside the href (the `RefDef.attrs` field + resolver merge).
||| A valid global attr like `lang` rides through the validity gate.
export
ext_refdef_nontitle_attr_flows_to_link : Property
ext_refdef_nontitle_attr_flows_to_link = oneShot $
  case parseDoc "{lang=fr}\n[ref]: /url\n\n[ref][]\n" of
    Left  e => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left  e            => failWith Nothing ("elaborate failed: " ++ show e)
      Right (h ** (_, _)) =>
        let out = renderHtml h
         in if isInfixOf "<a href=\"/url\" lang=\"fr\">ref</a>" out
              then success
              else failWith Nothing ("unexpected render: " ++ out)

||| End-to-end: an empty reference definition (`[link]:`) resolves a
||| collapsed `[link][]` to a link with an empty destination, which the
||| elaborator defaults to `href="#"` — the document elaborates cleanly
||| instead of erroring on `link-empty-href` (bucket C, corpus
||| links-and-images-007).
export
ext_empty_refdef_link_elaborates_clean : Property
ext_empty_refdef_link_elaborates_clean = oneShot $
  case parseDoc "[link][]\n\n[link]:\n[link2]: url\n" of
    Left  e => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left  e            => failWith Nothing ("elaborate failed: " ++ show e)
      Right (h ** (_, _)) =>
        let out = renderHtml h
         in if isInfixOf "<a href=\"#\">link</a>" out
              then success
              else failWith Nothing ("unexpected render: " ++ out)

||| A link whose only content is an image derives its accessible name
||| from the image's `alt`, so the full elaborate (with the StructuralAA
||| gate) accepts `<a href><img alt="image"></a>` (corpus -017/-024).
||| Kills the mutant that would drop `img` alt from `collectText`.
export
ext_image_in_link_is_accessible : Property
ext_image_in_link_is_accessible = oneShot $
  case elaborate (MkDoc [Paragraph emptyAttrs
                          [InlLink emptyAttrs (LinkInline "url" Nothing)
                             [InlImage emptyAttrs
                                (LinkInline "img.jpg" Nothing)
                                [InlText "image"]]]]) of
    Left  e => failWith Nothing ("elaborate failed: " ++ show e)
    Right _ => success

||| Parsed-then-elaborated image is a void `<img alt src>`, valid HTML
||| and AA-conformant. Pins both the parser/elaborator collaboration
||| and the void-element invariant for images.
export
ext_image_round_trip_through_elaborate : Property
ext_image_round_trip_through_elaborate = oneShot $
  case elaborate (MkDoc [Paragraph emptyAttrs
                          [InlImage emptyAttrs
                             (LinkInline "/x.png" Nothing)
                             [InlText "cat"]]]) of
    Left  e => failWith Nothing ("elaborate failed: " ++ show e)
    Right _ => success

--------------------------------------------------------------------------------
-- Tables.
--------------------------------------------------------------------------------

||| Rows render directly inside `<table>` (NO `<thead>`/`<tbody>`
||| wrappers, matching the Djot reference renderer). A body-only table
||| is `<table><tr><td>...</td></tr></table>`.
export
ext_table_body_only_emit : Property
ext_table_body_only_emit = oneShot $
  elaborateBlock
    (Table emptyAttrs Nothing
       [ MkRow False [ MkCell AlignNone [InlText "a"]
                     , MkCell AlignNone [InlText "b"]
                     ]
       ])
    === Element "table" []
          [ Element "tr" []
              [ Element "td" [] [Text "a"]
              , Element "td" [] [Text "b"]
              ]
          ]

||| Header + body rows render in source order directly under `<table>`.
||| Header rows use `<th>`, body rows `<td>`. A non-`AlignNone` cell
||| carries `style="text-align: <dir>;"` (space after colon, trailing
||| semicolon — the reference renderer's exact format).
export
ext_table_with_header_and_alignment_emit : Property
ext_table_with_header_and_alignment_emit = oneShot $
  elaborateBlock
    (Table emptyAttrs Nothing
       [ MkRow True  [ MkCell AlignLeft   [InlText "h1"]
                     , MkCell AlignCenter [InlText "h2"]
                     , MkCell AlignRight  [InlText "h3"]
                     ]
       , MkRow False [ MkCell AlignLeft   [InlText "x"]
                     , MkCell AlignCenter [InlText "y"]
                     , MkCell AlignRight  [InlText "z"]
                     ]
       ])
    === Element "table" []
          [ Element "tr" []
              [ Element "th" [MkHAttr "style" (Str "text-align: left;")]
                  [Text "h1"]
              , Element "th" [MkHAttr "style" (Str "text-align: center;")]
                  [Text "h2"]
              , Element "th" [MkHAttr "style" (Str "text-align: right;")]
                  [Text "h3"]
              ]
          , Element "tr" []
              [ Element "td" [MkHAttr "style" (Str "text-align: left;")]
                  [Text "x"]
              , Element "td" [MkHAttr "style" (Str "text-align: center;")]
                  [Text "y"]
              , Element "td" [MkHAttr "style" (Str "text-align: right;")]
                  [Text "z"]
              ]
          ]

||| A table with a caption leads with a `<caption>` containing the
||| parsed inline content, before the rows.
export
ext_table_caption_emit : Property
ext_table_caption_emit = oneShot $
  elaborateBlock
    (Table emptyAttrs (Just [InlText "cap"])
       [ MkRow False [MkCell AlignNone [InlText "a"]] ])
    === Element "table" []
          [ Element "caption" [] [Text "cap"]
          , Element "tr" [] [ Element "td" [] [Text "a"] ]
          ]

||| End-to-end: a parsed Djot table strict-elaborates without error.
||| Pins the table-shape -> valid-HTML + structural-AA contract.
export
ext_table_round_trip_strict : Property
ext_table_round_trip_strict = oneShot $
  case parseDoc "| h1 | h2 |\n|----|----|\n| a  | b  |" of
    Left  e => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborate d of
      Right _ => success
      Left  e => failWith Nothing ("elaborate: " ++ show e)

--------------------------------------------------------------------------------
-- Invisible blocks (RefDef + FootnoteDef).
--------------------------------------------------------------------------------

||| Reference-definition blocks contribute no visible output. The
||| reference Djot renderer emits nothing for them; Cribrum's
||| elaborator drops them via `isInvisibleBlock` at `elaborateDoc`.
||| Pins the filter so an inadvertent comment doesn't sneak back in.
export
ext_refdef_invisible : Property
ext_refdef_invisible = oneShot $
  elaborateDoc (MkDoc [RefDef "h" "https://example.org" Nothing emptyAttrs])
    === Element "main" [] []

||| Footnote definitions are now *visible*: each elaborates to a
||| semantic `<aside class="footnote" id="fn-<label>">` (convention §1),
||| label-anchored so the matching `InlFootnoteRef` anchor can target it.
export
ext_footnotedef_becomes_labelled_aside : Property
ext_footnotedef_becomes_labelled_aside = oneShot $
  elaborateDoc (MkDoc [FootnoteDef emptyAttrs "n" [para "a note"]])
    === Element "main" []
          [ Element "aside"
              [ MkHAttr "class" (Str "footnote")
              , MkHAttr "id" (Str "fn-n")
              ]
              [Element "p" [] [Text "a note"]] ]

||| A footnote reference `[^label]` becomes a `<sup>` anchor pointing at
||| the `fn-<label>` aside.
export
ext_footnote_ref_becomes_anchor : Property
ext_footnote_ref_becomes_anchor = oneShot $
  elaborateInline (InlFootnoteRef "n")
    === Element "a" [MkHAttr "href" (Str "#fn-n")]
          [Element "sup" [] [Text "n"]]

||| A document pairing a footnote reference with its definition strictly
||| elaborates — the emitted anchor + `<aside>` satisfy
||| `IsValidHtml × StructuralAA` (anchor has a non-empty href and an
||| accessible name via the `<sup>` label).
export
ext_footnote_ref_def_round_trip_strict : Property
ext_footnote_ref_def_round_trip_strict = oneShot $
  case elaborate (MkDoc [ Paragraph emptyAttrs
                            [InlText "see ", InlFootnoteRef "n"]
                        , FootnoteDef emptyAttrs "n" [para "the note"]
                        ]) of
    Right _ => success
    Left  e => failWith Nothing ("expected Right; got " ++ show e)

||| A document with both a paragraph and a RefDef elaborates to a
||| `<main>` containing only the paragraph — the RefDef contributes
||| nothing.
export
ext_paragraph_with_refdef_only_emits_paragraph : Property
ext_paragraph_with_refdef_only_emits_paragraph = oneShot $
  elaborateDoc (MkDoc [ para "hi"
                      , RefDef "h" "https://example.org" Nothing emptyAttrs
                      ])
    === Element "main" [] [Element "p" [] [Text "hi"]]

export
pddt_inline_mapping : Property
pddt_inline_mapping = withTests 1 . property $ do
  for_ inlineMappingCases $ \(input, expected) =>
    elaborateInline input === expected

--------------------------------------------------------------------------------
-- PBTs.
--------------------------------------------------------------------------------

genHeadingLevel : Gen Nat
genHeadingLevel = nat $ constant 1 6

genSimpleInlines : Gen (List Inline)
genSimpleInlines = list (linear 0 4) $
  choice $ the (Vect _ (Gen Inline))
    [ InlText <$> string (linear 0 8) ascii
    , pure InlSoftBreak
    , pure InlHardBreak
    , (\s => InlEmph   [InlText s]) <$> string (linear 0 4) ascii
    , (\s => InlStrong [InlText s]) <$> string (linear 0 4) ascii
    ]

||| Clamp any heading sequence so it satisfies the Phase-4 heading-no-skip
||| rule (first heading any level; each subsequent heading <= prev + 1). The
||| free generator otherwise emits skips like `[h1, h3]` which strict
||| elaboration now (rightly) rejects.
normalizeHeadings : List Block -> List Block
normalizeHeadings = go Nothing
  where
    go : Maybe Nat -> List Block -> List Block
    go _    []        = []
    go prev (Heading attrs lvl is :: rest) =
      let lvl' = case prev of
            Nothing => lvl
            Just p  => if lvl > S p then S p else lvl
       in Heading attrs lvl' is :: go (Just lvl') rest
    go prev (b :: rest) = b :: go prev rest

genSimpleBlocks : Gen (List Block)
genSimpleBlocks = normalizeHeadings <$> (list (linear 0 4) $
  choice $ the (Vect _ (Gen Block))
    [ Paragraph emptyAttrs <$> genSimpleInlines
    , [| Heading (pure emptyAttrs) genHeadingLevel genSimpleInlines |]
    , pure (ThematicBreak emptyAttrs)
    ])

genSimpleDoc : Gen Doc
genSimpleDoc = MkDoc <$> genSimpleBlocks

||| Strict-mode contract: every Doc produced by `genSimpleDoc` elaborates
||| without error, i.e. lands in the proof-carrying codomain.
export
pbt_strict_elaboration_total_on_simple_docs : Property
pbt_strict_elaboration_total_on_simple_docs = property $ do
  d <- forAll genSimpleDoc
  case elaborate d of
    Right _ => success
    Left  e => failWith Nothing ("unexpected failure: " ++ show e)

||| Every elaborated document is a valid HTML tree (regardless of slice).
export
pbt_elaborated_doc_isValidHtml : Property
pbt_elaborated_doc_isValidHtml = property $ do
  d <- forAll genSimpleDoc
  isValidHtml (elaborateDoc d) === True

||| Round-trip: parse arbitrary safe ASCII text, elaborate, get valid HTML.
||| Restricted to inputs that exercise only the parser's current slice.
genParserSafeInput : Gen String
genParserSafeInput = do
  let alphaChar = element $ the (Vect _ Char)
        ['a','b','c','d','e','f','g','h','i','j','k','l','m'
        ,'n','o','p','q','r','s','t','u','v','w','x','y','z'
        ,'0','1','2','3','4','5','6','7','8','9',' ']
  -- a sequence of short non-blank lines separated by blank lines
  lines <- list (linear 0 4) $
    [| pack
         ((::) <$> element (the (Vect _ Char) ['a','b','c','x','y','z'])
               <*> list (linear 0 8) alphaChar) |]
  pure (joinWithBlank lines)
  where
    joinWithBlank : List String -> String
    joinWithBlank []        = ""
    joinWithBlank [x]       = x
    joinWithBlank (x :: xs) = x ++ "\n\n" ++ joinWithBlank xs

export
pbt_parse_elaborate_round_trip : Property
pbt_parse_elaborate_round_trip = property $ do
  s <- forAll genParserSafeInput
  case parseDoc s of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Right _ => success
      Left  e => failWith Nothing ("elaborate failed: " ++ show e)

--------------------------------------------------------------------------------
-- Task lists + definition lists (P5.4 remainder slice).
--------------------------------------------------------------------------------

export
ext_task_list_emits_class_attrs : Property
ext_task_list_emits_class_attrs = oneShot $
  -- TaskList items unwrap a single Paragraph body so the rendered
  -- `<li class="…">` directly contains its inline content (matches
  -- the reference Djot renderer's tight-task-list output).
  elaborateDoc
    (doc [ListBlock emptyAttrs TaskList Nothing True
            [ MkLI emptyAttrs (Just False) Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            , MkLI emptyAttrs (Just True) Nothing
                [Paragraph emptyAttrs [InlText "two"]]
            ]])
    === Element "main" []
          [ Element "ul" [MkHAttr "class" (Str "task-list")]
              [ Element "li" [MkHAttr "class" (Str "unchecked")]
                  [Text "one"]
              , Element "li" [MkHAttr "class" (Str "checked")]
                  [Text "two"]
              ]]

||| A list's own `Attrs` (e.g. a `{.special}` block prefix) surface as
||| HTML attributes on the list element, emitted before any `start`/`type`
||| attribute (corpus attributes-021).
export
ext_list_attrs_emit_on_element : Property
ext_list_attrs_emit_on_element = oneShot $
  elaborateDoc
    (doc [ListBlock (MkAttrs Nothing ["special"] []) OrderedDecimal Nothing True
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            , MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "two"]]
            ]])
    === Element "main" []
          [ Element "ol" [MkHAttr "class" (Str "special")]
              [ Element "li" [] [Text "one"]
              , Element "li" [] [Text "two"]
              ]]

export
ext_definition_list_emits_dl_dt_dd : Property
ext_definition_list_emits_dl_dt_dd = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs Definition Nothing False
            [ MkLI emptyAttrs Nothing (Just [InlText "apple"])
                [Paragraph emptyAttrs [InlText "red fruit"]]
            , MkLI emptyAttrs Nothing (Just [InlText "orange"]) []
            ]])
    === Element "main" []
          [ Element "dl" []
              [ Element "dt" [] [Text "apple"]
              , Element "dd" []
                  [Element "p" [] [Text "red fruit"]]
              , Element "dt" [] [Text "orange"]
              ]]

||| A tight unordered list with single-`Paragraph` items emits each
||| item's inline content directly inside `<li>` (no `<p>` wrap). Pins
||| the `if tight` branch of `elabItem` so the collapse can't be
||| disabled or always-on without the test catching it.
export
ext_tight_ul_collapses_paragraph_wrap : Property
ext_tight_ul_collapses_paragraph_wrap = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs UnorderedDash Nothing True
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            , MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "two"]]
            ]])
    === Element "main" []
          [ Element "ul" []
              [ Element "li" [] [Text "one"]
              , Element "li" [] [Text "two"]
              ]]

||| A loose unordered list keeps the `<p>` wrap around each item's
||| paragraph. Pins the False branch of `if tight` — without this
||| test, a mutant that always collapses would still pass the suite.
export
ext_loose_ul_keeps_paragraph_wrap : Property
ext_loose_ul_keeps_paragraph_wrap = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs UnorderedDash Nothing False
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            ]])
    === Element "main" []
          [ Element "ul" []
              [ Element "li" [] [Element "p" [] [Text "one"]]
              ]]

||| An ordered list whose first marker is not 1 emits a `start=`
||| attribute. Pins the `startAttr` branch of the `ListBlock` elaborator.
export
ext_ordered_list_emits_start : Property
ext_ordered_list_emits_start = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs OrderedDecimal (Just 4) True
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            ]])
    === Element "main" []
          [ Element "ol" [MkHAttr "start" (Str "4")]
              [ Element "li" [] [Text "one"] ]]

||| A lower-roman ordered list emits `type="i"`, and `start` + `type`
||| appear in that order. Pins `typeAttr` and the `startAttr ++ typeAttr`
||| ordering in `olAttrs`.
export
ext_ordered_list_emits_type_and_start : Property
ext_ordered_list_emits_type_and_start = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs OrderedRomanLower (Just 41) True
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "one"]]
            ]])
    === Element "main" []
          [ Element "ol"
              [ MkHAttr "start" (Str "41"), MkHAttr "type" (Str "i") ]
              [ Element "li" [] [Text "one"] ]]

||| An unordered list never carries `start`/`type` even when the AST
||| (defensively) records a start. Pins the `tag == "ol"` guard.
export
ext_unordered_list_no_ol_attrs : Property
ext_unordered_list_no_ol_attrs = oneShot $
  elaborateDoc
    (doc [ListBlock emptyAttrs UnorderedDash Nothing True
            [ MkLI emptyAttrs Nothing Nothing
                [Paragraph emptyAttrs [InlText "x"]]
            ]])
    === Element "main" []
          [ Element "ul" []
              [ Element "li" [] [Text "x"] ]]

||| Attribute emission order is class first, then id, then key=val
||| pairs. Pins `attrsToHAttrs`' concatenation order so a mutant
||| flipping `classAttr ++ idAttr ++ others` to `idAttr ++ classAttr`
||| breaks the test.
export
ext_attrs_emit_class_before_id : Property
ext_attrs_emit_class_before_id = oneShot $
  elaborateDoc
    (doc [Paragraph (MkAttrs (Just "i") ["c"] [])
                    [InlText "x"]])
    === Element "main" []
          [ Element "p"
              [ MkHAttr "class" (Str "c")
              , MkHAttr "id"    (Str "i")
              ]
              [Text "x"]]

--------------------------------------------------------------------------------
-- Convention §2: fenced-div → semantic-element promotion.
--------------------------------------------------------------------------------

||| `:::nav` promotes to `<nav>` and the convention class is CONSUMED —
||| it never leaks into a `class` attribute (no-`div`-soup commitment).
export
ext_div_nav_promotes_and_consumes_class : Property
ext_div_nav_promotes_and_consumes_class = oneShot $
  elaborateBlock (divc ["nav"] [para "x"])
    === Element "nav" [] [Element "p" [] [Text "x"]]

export
ext_div_aside_promotes : Property
ext_div_aside_promotes = oneShot $
  elaborateBlock (divc ["aside"] [para "n"])
    === Element "aside" [] [Element "p" [] [Text "n"]]

||| `:::figure` wraps a nested `:::figcaption`; each div promotes
||| independently to its own semantic element.
export
ext_div_figure_with_figcaption_nests : Property
ext_div_figure_with_figcaption_nests = oneShot $
  elaborateBlock (divc ["figure"] [divc ["figcaption"] [para "cap"]])
    === Element "figure" []
          [Element "figcaption" [] [Element "p" [] [Text "cap"]]]

export
ext_div_header_promotes : Property
ext_div_header_promotes = oneShot $
  elaborateBlock (divc ["header"] [para "h"])
    === Element "header" [] [Element "p" [] [Text "h"]]

export
ext_div_footer_promotes : Property
ext_div_footer_promotes = oneShot $
  elaborateBlock (divc ["footer"] [para "f"])
    === Element "footer" [] [Element "p" [] [Text "f"]]

||| `:::main` is the explicit override of the §3 `<main>`-wrapper
||| inference.
export
ext_div_main_promotes : Property
ext_div_main_promotes = oneShot $
  elaborateBlock (divc ["main"] [para "m"])
    === Element "main" [] [Element "p" [] [Text "m"]]

||| `:::section` is the explicit override of the §1b heading→`<section>`
||| inference.
export
ext_div_section_promotes : Property
ext_div_section_promotes = oneShot $
  elaborateBlock (divc ["section"] [para "s"])
    === Element "section" [] [Element "p" [] [Text "s"]]

||| A div whose classes are all non-convention stays a plain `<div>`
||| and keeps every class. (Mutant-kill on `promoteDiv`'s default.)
export
ext_div_non_convention_stays_div : Property
ext_div_non_convention_stays_div = oneShot $
  elaborateBlock (divc ["note", "warning"] [para "x"])
    === Element "div" [MkHAttr "class" (Str "note warning")]
          [Element "p" [] [Text "x"]]

||| The FIRST convention class (source order) drives promotion and is
||| dropped; non-convention classes around it survive in order. Kills
||| mutants that consume the wrong class or stop scanning early.
export
ext_div_first_convention_wins_residual_classes_kept : Property
ext_div_first_convention_wins_residual_classes_kept = oneShot $
  elaborateBlock (divc ["sidebar", "nav", "sticky"] [para "x"])
    === Element "nav" [MkHAttr "class" (Str "sidebar sticky")]
          [Element "p" [] [Text "x"]]

||| `{role=}` / `{lang=}` annotations ride through untouched as pair
||| attributes on the promoted element.
export
ext_div_role_lang_ride_through : Property
ext_div_role_lang_ride_through = oneShot $
  elaborateBlock
    (Div (MkAttrs Nothing ["aside"] [("role", "note"), ("lang", "fr")])
         [para "x"])
    === Element "aside"
          [ MkHAttr "role" (Str "note")
          , MkHAttr "lang" (Str "fr")
          ]
          [Element "p" [] [Text "x"]]

--------------------------------------------------------------------------------
-- Convention §2 (span side): inline-span → semantic-phrasing promotion.
--------------------------------------------------------------------------------

||| `[..]{.abbr}` promotes to `<abbr>` and the convention class is
||| CONSUMED — never leaks into a `class` attribute (no-`span`-soup).
export
ext_span_abbr_promotes_and_consumes_class : Property
ext_span_abbr_promotes_and_consumes_class = oneShot $
  elaborateInline (spanc ["abbr"] [InlText "HTML"])
    === Element "abbr" [] [Text "HTML"]

export
ext_span_cite_promotes : Property
ext_span_cite_promotes = oneShot $
  elaborateInline (spanc ["cite"] [InlText "Moby-Dick"])
    === Element "cite" [] [Text "Moby-Dick"]

export
ext_span_kbd_promotes : Property
ext_span_kbd_promotes = oneShot $
  elaborateInline (spanc ["kbd"] [InlText "Ctrl"])
    === Element "kbd" [] [Text "Ctrl"]

||| A span whose classes are all non-convention stays a plain `<span>`
||| and keeps every class. Kills the `promoteSpan` default mutant and
||| guards against the prior whole-attribute-block drop.
export
ext_span_non_convention_stays_span : Property
ext_span_non_convention_stays_span = oneShot $
  elaborateInline (spanc ["note", "warning"] [InlText "x"])
    === Element "span" [MkHAttr "class" (Str "note warning")] [Text "x"]

||| The FIRST convention class (source order) drives promotion and is
||| dropped; non-convention classes around it survive in order. Kills
||| mutants that consume the wrong class or stop scanning early.
export
ext_span_first_convention_wins_residual_classes_kept : Property
ext_span_first_convention_wins_residual_classes_kept = oneShot $
  elaborateInline (spanc ["lead", "cite", "fancy"] [InlText "x"])
    === Element "cite" [MkHAttr "class" (Str "lead fancy")] [Text "x"]

||| `id` / `{role=}` / `{lang=}` annotations ride through untouched on
||| the promoted element. Direct regression for the prior bug where
||| `InlSpan` dropped its entire `Attrs`.
export
ext_span_id_role_lang_ride_through : Property
ext_span_id_role_lang_ride_through = oneShot $
  elaborateInline
    (InlSpan (MkAttrs (Just "t1") ["time"] [("role", "note"), ("lang", "fr")])
             [InlText "x"])
    === Element "time"
          [ MkHAttr "id" (Str "t1")
          , MkHAttr "role" (Str "note")
          , MkHAttr "lang" (Str "fr")
          ]
          [Text "x"]

--------------------------------------------------------------------------------
-- Convention §3: main-wrapper inference with explicit override.
--------------------------------------------------------------------------------

||| Inference-with-override: when the whole document body is an explicit
||| `:::main` (promoted to `<main>`), the inferred `<main>` wrapper steps
||| aside — the document is the explicit landmark itself, not a `<main>`
||| nested in a `<main>`.
export
ext_explicit_main_not_double_wrapped : Property
ext_explicit_main_not_double_wrapped = oneShot $
  elaborateDoc (MkDoc [divc ["main"] [para "x"]])
    === Element "main" [] [Element "p" [] [Text "x"]]

||| Consequence of the override: a `:::main`-rooted document strictly
||| elaborates. Without the override it would emit `<main><main>…` and
||| fail the `unique-main` structural rule.
export
ext_explicit_main_strict_no_nested_main : Property
ext_explicit_main_strict_no_nested_main = oneShot $
  case elaborate (MkDoc [divc ["main"] [para "x"]]) of
    Right _ => success
    Left  e => failWith Nothing ("expected Right; got " ++ show e)

||| The wrapper still fires for ordinary bodies: a plain paragraph is
||| wrapped in the inferred `<main>` (override only triggers on a sole
||| explicit `<main>` child).
export
ext_plain_body_keeps_inferred_main : Property
ext_plain_body_keeps_inferred_main = oneShot $
  elaborateDoc (MkDoc [para "x"])
    === Element "main" [] [Element "p" [] [Text "x"]]

||| `{role=main}` on a body's sole element is an explicit main landmark
||| (the ARIA form). Like `:::main`, the inferred `<main>` wrapper steps
||| aside so the document is the single landmark itself — wrapping would
||| produce a `<main>` enclosing a `role="main"` element (two mains).
export
ext_role_main_div_not_double_wrapped : Property
ext_role_main_div_not_double_wrapped = oneShot $
  elaborateDoc (MkDoc [Div (MkAttrs Nothing [] [("role", "main")]) [para "x"]])
    === Element "div" [MkHAttr "role" (Str "main")] [Element "p" [] [Text "x"]]

||| Consequence of the `{role=main}` override: such a document strictly
||| elaborates. Without it the inferred `<main>` wrapper + the ARIA main
||| role would be two main landmarks.
export
ext_role_main_strict_elaborates : Property
ext_role_main_strict_elaborates = oneShot $
  case elaborate (MkDoc [Div (MkAttrs Nothing [] [("role", "main")]) [para "x"]]) of
    Right _ => success
    Left  e => failWith Nothing ("expected Right; got " ++ show e)

--------------------------------------------------------------------------------
-- Convention §1: raw-block / raw-inline format gating.
--------------------------------------------------------------------------------

||| A `=html` raw block injects its body verbatim as a `Raw` passthrough
||| node (no escaping, no wrapping element).
export
ext_raw_block_html_passthrough : Property
ext_raw_block_html_passthrough = oneShot $
  elaborateBlock (RawBlock "html" "<table>")
    === Raw "<table>"

||| A raw block of any non-`html` format is suppressed — no visible output.
export
ext_raw_block_other_format_suppressed : Property
ext_raw_block_other_format_suppressed = oneShot $
  elaborateBlock (RawBlock "latex" "\\begin{x}")
    === Text ""

||| A `=html` raw inline injects its content verbatim as a `Raw` node.
export
ext_raw_inline_html_passthrough : Property
ext_raw_inline_html_passthrough = oneShot $
  elaborateInline (InlRaw "html" "<a>")
    === Raw "<a>"

||| A raw inline of any non-`html` format is suppressed.
export
ext_raw_inline_other_format_suppressed : Property
ext_raw_inline_other_format_suppressed = oneShot $
  elaborateInline (InlRaw "tex" "\\(x\\)")
    === Text ""

||| End-to-end: `=html` fenced block parses + elaborates to a `Raw` node
||| carrying the literal body, and the whole document strictly elaborates.
export
ext_raw_block_round_trip_strict : Property
ext_raw_block_round_trip_strict = oneShot $
  case parseDoc "``` =html\n<table>\n```\n" of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left e               => failWith Nothing ("elaborate failed: " ++ show e)
      Right (Element "main" [] [Raw "<table>"] ** _) => success
      Right (h ** _)                                 =>
        failWith Nothing ("unexpected tree: " ++ show h)

||| End-to-end through the *parser*: `` `<a>`{=html} `` parses to an
||| `InlRaw "html" "<a>"` and elaborates to a `Raw` node inside the `<p>`.
||| Exercises `takeRawFormatAttr` (the parser-side raw-inline detector),
||| which the direct `elaborateInline` tests above do not reach.
export
ext_raw_inline_round_trip_strict : Property
ext_raw_inline_round_trip_strict = oneShot $
  case parseDoc "`<a>`{=html}\n" of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left e => failWith Nothing ("elaborate failed: " ++ show e)
      Right (Element "main" [] [Element "p" [] [Raw "<a>"]] ** _) => success
      Right (h ** _) => failWith Nothing ("unexpected tree: " ++ show h)

||| Two adjacent ordered items with DIFFERENT delimiters (`1.` then `1)`)
||| are two separate lists, not one — the reference parser starts a fresh
||| list on a delimiter change. (Mutant-kill on the `delim x == delim y`
||| same-list test; with it forced True the two items would merge into a
||| single `<ol>`.)
export
ext_ordered_different_delims_split : Property
ext_ordered_different_delims_split = oneShot $
  case parseDoc "1. a\n1) b\n" of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborateDoc d of
      Element "main" _ cs => length (filter isOl cs) === 2
      other => failWith Nothing ("not a <main> root: " ++ show other)
  where
    isOl : HExpr -> Bool
    isOl (Element "ol" _ _) = True
    isOl _                  = False

||| A decorator highlight whose body itself contains the marker char
||| (`{=a=b=}` -> `<mark>a=b</mark>`): the closer is the two-char `=}`, so
||| the lone interior `=` must NOT close the span. (Mutant-kill on
||| `findClose2`'s `x == c1 && y == c2`; with `&&` weakened to `||` the
||| span would close early at the interior `=`, dropping `b`.)
export
ext_highlight_interior_marker_closes_on_pair : Property
ext_highlight_interior_marker_closes_on_pair = oneShot $
  case parseDoc "{=a=b=}\n" of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => elaborateDoc d
      === Element "main" []
            [Element "p" [] [Element "mark" [] [Text "a=b"]]]

--------------------------------------------------------------------------------
-- Strict semantic errors + draft-mode placeholder repair.
--------------------------------------------------------------------------------

||| (1a, strict) A disallowed author attribute is a hard error carrying a
||| clear, localized, *semantic* message — not just a raw constructor dump.
export
ext_strict_disallowed_attr_explained : Property
ext_strict_disallowed_attr_explained = oneShot $
  case parseDoc "hi{key=\"x\"}\n" of
    Left e  => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborate d of
      Right _ => failWith Nothing "expected a strict rejection"
      Left e  =>
        let s = show e
         in (isInfixOf "not a valid HTML attribute" s
               && isInfixOf "key" s
               && isInfixOf "span" s) === True

||| (1a, draft) The same attribute is repaired to `data-key` (value
||| preserved) + flagged with a `disallowed-attr` warning; the tree is
||| proof-carrying (the `Right` carries `IsValidHtml`/`StructuralAA`).
export
ext_draft_disallowed_attr_renamed : Property
ext_draft_disallowed_attr_renamed = oneShot $
  case parseDoc "hi{key=\"x\"}\n" of
    Left e  => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborateDraft d of
      Left e => failWith Nothing ("draft should repair, got: " ++ show e)
      Right (h ** (_, _, ws)) =>
        (isInfixOf "data-key=\"x\"" (renderHtml h)
           && any (\w => w.rule == "disallowed-attr") ws) === True

||| (1b, draft) A nested `<a>` is demoted to `<span>` + flagged.
export
ext_draft_nested_anchor_demoted : Property
ext_draft_nested_anchor_demoted = oneShot $
  case parseDoc "[[foo](bar)](baz)\n" of
    Left e  => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborateDraft d of
      Left e => failWith Nothing ("draft should repair, got: " ++ show e)
      Right (_ ** (_, _, ws)) =>
        any (\w => w.rule == "interactive-in-interactive") ws === True

||| (2b, draft) An empty href is filled with a `#` placeholder + flagged.
export
ext_draft_empty_href_filled : Property
ext_draft_empty_href_filled = oneShot $
  case parseDoc "[link][]\n\n[link]:\n" of
    Left e  => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborateDraft d of
      Left e => failWith Nothing ("draft should repair, got: " ++ show e)
      Right (h ** (_, _, ws)) =>
        (isInfixOf "href=\"#\"" (renderHtml h)
           && any (\w => w.rule == "link-empty-href") ws) === True

export
group : Group
group = MkGroup "Cribrum.Elaborate"
  [ ("ext_empty_doc_elaborates_to_empty_main", ext_empty_doc_elaborates_to_empty_main)
  , ("ext_paragraph_becomes_p",                ext_paragraph_becomes_p)
  , ("ext_heading_levels_map_to_h_tags",       ext_heading_levels_map_to_h_tags)
  , ("ext_sibling_headings_are_sibling_sections", ext_sibling_headings_are_sibling_sections)
  , ("ext_deeper_heading_nests",               ext_deeper_heading_nests)
  , ("ext_thematic_becomes_hr",                ext_thematic_becomes_hr)
  , ("ext_softbreak_becomes_space",            ext_softbreak_becomes_space)
  , ("ext_hardbreak_becomes_br",               ext_hardbreak_becomes_br)
  , ("ext_emphasis_becomes_em",                ext_emphasis_becomes_em)
  , ("ext_strong_becomes_strong",              ext_strong_becomes_strong)
  , ("ext_strict_elaboration_carries_proof",   ext_strict_elaboration_carries_proof)
  , ("ext_round_trip_parse_then_elaborate",    ext_round_trip_parse_then_elaborate)
  , ("ext_strict_elaboration_rejects_skipped_headings",
        ext_strict_elaboration_rejects_skipped_headings)
  , ("ext_per_node_rule_failure_is_located",
        ext_per_node_rule_failure_is_located)
  , ("ext_unique_main_failure_whole_tree",     ext_unique_main_failure_whole_tree)
  , ("pddt_heading_tags",                      pddt_heading_tags)
  , ("pddt_inline_mapping",                    pddt_inline_mapping)
  , ("ext_inline_math_span",                   ext_inline_math_span)
  , ("ext_display_math_span",                  ext_display_math_span)
  , ("ext_image_alt_flattens_emphasis",        ext_image_alt_flattens_emphasis)
  , ("ext_link_title_emitted",                 ext_link_title_emitted)
  , ("ext_link_inline_title_overrides_ref",     ext_link_inline_title_overrides_ref)
  , ("ext_link_empty_href_defaults_to_hash",    ext_link_empty_href_defaults_to_hash)
  , ("ext_link_carries_valid_author_attr",      ext_link_carries_valid_author_attr)
  , ("ext_refdef_attr_title_flows_to_link",     ext_refdef_attr_title_flows_to_link)
  , ("ext_refdef_nontitle_attr_flows_to_link",  ext_refdef_nontitle_attr_flows_to_link)
  , ("ext_empty_refdef_link_elaborates_clean",  ext_empty_refdef_link_elaborates_clean)
  , ("ext_image_in_link_is_accessible",        ext_image_in_link_is_accessible)
  , ("ext_image_round_trip_through_elaborate", ext_image_round_trip_through_elaborate)
  , ("ext_table_body_only_emit",               ext_table_body_only_emit)
  , ("ext_table_with_header_and_alignment_emit",
        ext_table_with_header_and_alignment_emit)
  , ("ext_table_caption_emit",                 ext_table_caption_emit)
  , ("ext_table_round_trip_strict",            ext_table_round_trip_strict)
  , ("ext_refdef_invisible",                   ext_refdef_invisible)
  , ("ext_footnotedef_becomes_labelled_aside", ext_footnotedef_becomes_labelled_aside)
  , ("ext_footnote_ref_becomes_anchor",        ext_footnote_ref_becomes_anchor)
  , ("ext_footnote_ref_def_round_trip_strict", ext_footnote_ref_def_round_trip_strict)
  , ("ext_paragraph_with_refdef_only_emits_paragraph",
        ext_paragraph_with_refdef_only_emits_paragraph)
  , ("pbt_strict_elaboration_total_on_simple_docs",
        pbt_strict_elaboration_total_on_simple_docs)
  , ("pbt_elaborated_doc_isValidHtml",         pbt_elaborated_doc_isValidHtml)
  , ("pbt_parse_elaborate_round_trip",         pbt_parse_elaborate_round_trip)
  , ("ext_task_list_emits_class_attrs",        ext_task_list_emits_class_attrs)
  , ("ext_list_attrs_emit_on_element",         ext_list_attrs_emit_on_element)
  , ("ext_definition_list_emits_dl_dt_dd",     ext_definition_list_emits_dl_dt_dd)
  , ("ext_tight_ul_collapses_paragraph_wrap",  ext_tight_ul_collapses_paragraph_wrap)
  , ("ext_loose_ul_keeps_paragraph_wrap",      ext_loose_ul_keeps_paragraph_wrap)
  , ("ext_ordered_list_emits_start",           ext_ordered_list_emits_start)
  , ("ext_ordered_list_emits_type_and_start",  ext_ordered_list_emits_type_and_start)
  , ("ext_unordered_list_no_ol_attrs",         ext_unordered_list_no_ol_attrs)
  , ("ext_attrs_emit_class_before_id",         ext_attrs_emit_class_before_id)
  , ("ext_div_nav_promotes_and_consumes_class", ext_div_nav_promotes_and_consumes_class)
  , ("ext_div_aside_promotes",                 ext_div_aside_promotes)
  , ("ext_div_figure_with_figcaption_nests",   ext_div_figure_with_figcaption_nests)
  , ("ext_div_header_promotes",                ext_div_header_promotes)
  , ("ext_div_footer_promotes",                ext_div_footer_promotes)
  , ("ext_div_main_promotes",                  ext_div_main_promotes)
  , ("ext_div_section_promotes",               ext_div_section_promotes)
  , ("ext_div_non_convention_stays_div",       ext_div_non_convention_stays_div)
  , ("ext_div_first_convention_wins_residual_classes_kept",
        ext_div_first_convention_wins_residual_classes_kept)
  , ("ext_div_role_lang_ride_through",         ext_div_role_lang_ride_through)
  , ("ext_span_abbr_promotes_and_consumes_class",
        ext_span_abbr_promotes_and_consumes_class)
  , ("ext_span_cite_promotes",                 ext_span_cite_promotes)
  , ("ext_span_kbd_promotes",                  ext_span_kbd_promotes)
  , ("ext_span_non_convention_stays_span",     ext_span_non_convention_stays_span)
  , ("ext_span_first_convention_wins_residual_classes_kept",
        ext_span_first_convention_wins_residual_classes_kept)
  , ("ext_span_id_role_lang_ride_through",     ext_span_id_role_lang_ride_through)
  , ("ext_explicit_main_not_double_wrapped",   ext_explicit_main_not_double_wrapped)
  , ("ext_explicit_main_strict_no_nested_main", ext_explicit_main_strict_no_nested_main)
  , ("ext_plain_body_keeps_inferred_main",     ext_plain_body_keeps_inferred_main)
  , ("ext_role_main_div_not_double_wrapped",   ext_role_main_div_not_double_wrapped)
  , ("ext_role_main_strict_elaborates",        ext_role_main_strict_elaborates)
  , ("ext_raw_block_html_passthrough",         ext_raw_block_html_passthrough)
  , ("ext_raw_block_other_format_suppressed",  ext_raw_block_other_format_suppressed)
  , ("ext_raw_inline_html_passthrough",        ext_raw_inline_html_passthrough)
  , ("ext_raw_inline_other_format_suppressed", ext_raw_inline_other_format_suppressed)
  , ("ext_raw_block_round_trip_strict",        ext_raw_block_round_trip_strict)
  , ("ext_raw_inline_round_trip_strict",       ext_raw_inline_round_trip_strict)
  , ("ext_ordered_different_delims_split",      ext_ordered_different_delims_split)
  , ("ext_highlight_interior_marker_closes_on_pair",
        ext_highlight_interior_marker_closes_on_pair)
  , ("ext_strict_disallowed_attr_explained",   ext_strict_disallowed_attr_explained)
  , ("ext_draft_disallowed_attr_renamed",      ext_draft_disallowed_attr_renamed)
  , ("ext_draft_nested_anchor_demoted",        ext_draft_nested_anchor_demoted)
  , ("ext_draft_empty_href_filled",            ext_draft_empty_href_filled)
  ]
