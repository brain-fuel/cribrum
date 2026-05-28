||| TEAWeb T6 demo — Cribrum docs navigation island.
|||
||| Per plan.dj §Phase T6: the keystone end-to-end exercise of every
||| TEAWeb layer. Mounted as a single interactive island into a static-
||| rendered docs page, exposing a table-of-contents pane with:
|||
|||   - search-as-you-type filter (`onInput` -> string filter)
|||   - keyboard navigation (ArrowUp / ArrowDown / Home / End)
|||   - Enter to navigate to the highlighted entry's anchor
|||   - mouse hover sets the highlight; click selects
|||
||| The TOC catalog is *generated* from `README.dj` by
||| `tools/render-docsnav` — see `Generated.idr` (auto-emitted) and
||| `TocData.idr` (record type). Anchors are the `autoId`s
||| `Cribrum.Pipeline.Anchor` derives from the heading text and match the
||| `id` attributes the same pipeline writes onto the wrapping
||| `<section>` elements in `index.html`, so in-page navigation works
||| without manual sync.
|||
||| Build:
|||   $ make docsnav             -- regenerates index.html + Generated.idr
|||   $ idris2 --cg javascript --build docsnav.ipkg
|||
||| Then open `index.html` in a browser — the bundle loads on
||| `window.onload` and mounts under `#toc-island`.
module Main

import Data.List
import Data.String
import TEAWeb.Html
import TEAWeb.Event
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Program
import TEAWeb.Runtime
import TocData
import Generated

%default total

--------------------------------------------------------------------------------
-- TOC catalog. Harvested from README.dj via the actual Cribrum pipeline
-- (parser -> elaborate -> addSectionIds -> harvestHeadings).
--------------------------------------------------------------------------------

tocItems : List TocItem
tocItems = genTocItems

--------------------------------------------------------------------------------
-- Msg + Model.
--------------------------------------------------------------------------------

data Msg
  = UpdateQuery String
  | KeyPressed  String
  | HoverItem   Nat
  | ClickItem   Nat
  | ClearQuery
  | FocusSearch

record Model where
  constructor MkModel
  query    : String
  -- Selected index within the *filtered* list (0-based). Nothing = no
  -- selection yet; Enter on Nothing is a no-op.
  selected : Maybe Nat

--------------------------------------------------------------------------------
-- Filter pipeline.
--------------------------------------------------------------------------------

||| ASCII-only case fold. Matches plan.dj's stance: no Unicode case folding
||| in the demo's search; this stays predictable across implementations.
toLowerAscii : Char -> Char
toLowerAscii c =
  let n = ord c
   in if n >= 65 && n <= 90 then chr (n + 32) else c

lowerStr : String -> String
lowerStr s = pack (map toLowerAscii (unpack s))

||| Substring match. `needle ⊆ haystack` after case fold.
substringMatch : String -> String -> Bool
substringMatch needle haystack =
  isInfixOf (lowerStr (trim needle)) (lowerStr haystack)

filterItems : String -> List TocItem -> List TocItem
filterItems q items =
  if trim q == ""
    then items
    else filter (\it => substringMatch q (title it)) items

--------------------------------------------------------------------------------
-- TEA functions.
--------------------------------------------------------------------------------

init_ : (Model, Cmd Msg)
init_ = (MkModel "" Nothing, None)

||| Clamp `n` to `[0, len - 1]`. If `len == 0`, returns `Nothing`.
clampIdx : Nat -> Nat -> Maybe Nat
clampIdx _   Z         = Nothing
clampIdx idx (S last)  = Just (if idx > last then last else idx)

||| Compute new highlight on arrow keys. `key` is one of
|||   "ArrowDown"  — move highlight down (wrap to 0 at end).
|||   "ArrowUp"    — move highlight up (wrap to last at 0).
|||   "Home"       — first item.
|||   "End"        — last item.
|||   "Escape"     — clear query.
|||   other        — no change.
moveSelected : String -> Maybe Nat -> Nat -> Maybe Nat
moveSelected _           _        Z = Nothing
moveSelected "ArrowDown" Nothing  _ = Just 0
moveSelected "ArrowDown" (Just i) (S last) =
  Just (if i >= last then 0 else S i)
moveSelected "ArrowUp"   Nothing  (S last) = Just last
moveSelected "ArrowUp"   (Just i) (S last) =
  Just (case i of
          Z   => last
          S k => k)
moveSelected "Home"      _        _       = Just 0
moveSelected "End"       _        (S last) = Just last
moveSelected _           prev     _       = prev

||| Currently-filtered TOC items for `model`.
visibleItems : Model -> List TocItem
visibleItems m = filterItems m.query tocItems

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ (UpdateQuery q) m =
  -- New query — reset selection to first hit if any.
  let filtered : List TocItem
      filtered = filterItems q tocItems
      sel      : Maybe Nat
      sel      = case filtered of
                   []     => Nothing
                   _ :: _ => Just 0
   in ({ query := q, selected := sel } m, None)
update_ (KeyPressed key) m =
  let len    = length (visibleItems m)
      newSel = moveSelected key m.selected len
   in case key of
        "Escape" => (MkModel "" Nothing, None)
        _        => ({ selected := newSel } m, None)
update_ (HoverItem i) m =
  ({ selected := Just i } m, None)
update_ (ClickItem i) m =
  -- Demo: click confirms a TOC selection; in a real app this would
  -- emit a Cmd to update the URL hash. We just set the selected index.
  ({ selected := Just i } m, None)
update_ ClearQuery m =
  (MkModel "" Nothing, Focus "toc-search")
update_ FocusSearch m =
  (m, Focus "toc-search")

--------------------------------------------------------------------------------
-- View.
--------------------------------------------------------------------------------

||| Indentation marker per heading level. h1 -> 0; h2 -> 1.
levelClass : Nat -> String
levelClass 1 = "toc-l1"
levelClass _ = "toc-l2"

||| Render one TOC entry. `idx` is the position within the filtered list
||| (used by event handlers and the highlight check).
viewItem : Maybe Nat -> Nat -> TocItem -> View Msg
viewItem sel idx it =
  let isSelected = sel == Just idx
      base       = ["toc-item", levelClass (level it)]
      cls        = if isSelected
                     then concat ["toc-item ", levelClass (level it), " selected"]
                     else concat ["toc-item ", levelClass (level it)]
   in li_ [class_ cls]
        [ a_ [ href_ ("#" ++ anchor it)
             , id_   ("toc-anchor-" ++ show idx)
             , onClick      ("toc-click-" ++ show idx) (ClickItem idx)
             , onMouseEnter ("toc-hover-" ++ show idx) (HoverItem idx)
             ]
            [ text_ (title it) ]
        ]

view_ : Model -> View Msg
view_ m =
  let items = visibleItems m
   in section_ [class_ "docsnav-island", role_ "navigation"]
        [ header_ [class_ "docsnav-header"]
            [ h2_ [] [text_ "Table of contents"]
            , p_  [class_ "docsnav-help"]
                [ text_ "Type to filter \xb7 \x2191/\x2193 navigate \xb7 Enter selects \xb7 Esc clears" ]
            ]
        , div_ [class_ "docsnav-search"]
            [ input_
                [ id_           "toc-search"
                , type_         "search"
                , placeholder_  "Filter sections..."
                , value_        m.query
                , onInput       "toc-input"   UpdateQuery
                , onKeyDown     "toc-keydown" KeyPressed
                ]
                []
            , button_
                [ id_      "toc-clear-btn"
                , onClick  "toc-clear" ClearQuery
                , type_    "button"
                ]
                [ text_ "Clear" ]
            ]
        , case items of
            [] => p_ [class_ "docsnav-empty"]
                    [ text_ ("No sections match \"" ++ m.query ++ "\".") ]
            _  => ul_ [ class_ "docsnav-list", id_ "toc-list" ]
                    (zipWithIndexFrom 0 (viewItem m.selected) items)
        , p_ [class_ "docsnav-status"]
            [ text_ (show (length items) ++ " of "
                       ++ show (length tocItems) ++ " sections shown") ]
        ]
  where
    -- Tail-recursive zip with explicit index — keeps the renderer total
    -- without conjuring Vect-style index machinery.
    zipWithIndexFrom : Nat -> (Nat -> a -> b) -> List a -> List b
    zipWithIndexFrom _ _ []        = []
    zipWithIndexFrom n f (x :: xs) = f n x :: zipWithIndexFrom (S n) f xs

subs_ : Model -> Sub Msg
subs_ _ = None

prog : Program Model Msg
prog = MkProgram init_ update_ view_ subs_

--------------------------------------------------------------------------------
-- Entry point. Mount under the DOM element with id "toc-island".
--------------------------------------------------------------------------------

main : IO ()
main = mount prog "toc-island"
