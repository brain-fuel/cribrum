||| HTML renderer — Phase 5 (string form; DOM form arrives with the JS-backend
||| pass). Total `HExpr -> String`.
|||
||| HTML 5 rules honoured:
|||
|||   - Void elements (br, hr, img, meta, link, input, ...) render as
|||     `<tag attrs>` with no closing tag and no children. If the input HExpr
|||     wrongly attaches children to a void element they are silently
|||     discarded by this renderer (the right place to catch this is
|||     `IsValidHtml`'s content model in a later Phase 2 iteration).
|||   - Text nodes are escaped: `&`, `<`, `>`, `"`, `'` are entitised.
|||   - Comments render as `<!-- ... -->`; no escaping inside.
|||   - Attribute values are surrounded by `"` and have `"`/`&` escaped.
|||   - Handler-valued attributes (`AttrValue.Handler`) are rendered as a
|||     `data-on-<event>="<callbackId>"` data attribute so the markup is
|||     deterministic and JS wiring can pick the handler up at hydration.
module Cribrum.Render.Html

import Data.List
import Data.String
import Cribrum.Node

%default total

--------------------------------------------------------------------------------
-- Escaping.
--------------------------------------------------------------------------------

escapeChar : Char -> String
escapeChar '&'  = "&amp;"
escapeChar '<'  = "&lt;"
escapeChar '>'  = "&gt;"
escapeChar '"'  = "&quot;"
escapeChar '\'' = "&#39;"
escapeChar c    = singleton c

public export
escapeText : String -> String
escapeText = concatMap escapeChar . unpack

attrValueChar : Char -> String
attrValueChar '&' = "&amp;"
attrValueChar '"' = "&quot;"
attrValueChar c   = singleton c

escapeAttrValue : String -> String
escapeAttrValue = concatMap attrValueChar . unpack

--------------------------------------------------------------------------------
-- Void element set (HTML 5).
--------------------------------------------------------------------------------

||| Void elements per the HTML 5 spec.
public export
voidElements : List String
voidElements =
  [ "area", "base", "br", "col", "embed", "hr"
  , "img", "input", "link", "meta", "source", "track", "wbr"
  ]

public export
isVoid : String -> Bool
isVoid t = elem t voidElements

--------------------------------------------------------------------------------
-- Attribute rendering.
--------------------------------------------------------------------------------

renderAttrValue : AttrValue -> (String, String)
renderAttrValue (Str s)         = ("", escapeAttrValue s)
renderAttrValue (Handler ev cb) = ("on-prefix", escapeAttrValue cb)

renderAttr : HAttr -> String
renderAttr (MkHAttr name (Str s)) =
  " " ++ name ++ "=\"" ++ escapeAttrValue s ++ "\""
renderAttr (MkHAttr _    (Handler ev cb)) =
  " data-on-" ++ ev ++ "=\"" ++ escapeAttrValue cb ++ "\""

renderAttrs : List HAttr -> String
renderAttrs = concatMap renderAttr

--------------------------------------------------------------------------------
-- Render.
--------------------------------------------------------------------------------

public export
renderHtml : HExpr -> String
renderHtml (Text s)         = escapeText s
renderHtml (Comment s)      = "<!-- " ++ s ++ " -->"
renderHtml (Element t a cs) =
  if isVoid t
    then "<" ++ t ++ renderAttrs a ++ ">"
    else "<" ++ t ++ renderAttrs a ++ ">"
           ++ concatMap (assert_total renderHtml) cs
           ++ "</" ++ t ++ ">"
