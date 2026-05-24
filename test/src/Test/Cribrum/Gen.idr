module Test.Cribrum.Gen

import Data.Vect
import Hedgehog
import Cribrum.Node

%default total

||| Small set of legal-looking HTML tags. Validity proofs live in Phase 2; this
||| only needs *plausible* tags so PBTs over HExpr exercise realistic shapes.
export
tagName : Gen String
tagName = element $ the (Vect _ String)
  [ "div", "span", "p", "section", "article", "nav", "aside"
  , "h1", "h2", "h3", "a", "ul", "li", "figure", "figcaption"
  , "img", "input", "label", "button"
  ]

export
attrName : Gen String
attrName = element $ the (Vect _ String)
  [ "id", "class", "href", "src", "alt", "title", "lang"
  , "role", "aria-label", "for", "name", "type"
  ]

export
attrValue : Gen AttrValue
attrValue = choice $ the (Vect _ (Gen AttrValue))
  [ Str <$> string (linear 0 12) ascii
  , [| Handler (element $ the (Vect _ String) ["click", "input", "submit"])
               (string (linear 1 8) alphaNum) |]
  ]

export
hattr : Gen HAttr
hattr = [| MkHAttr attrName attrValue |]

||| Tree generator with bounded depth so test runs terminate quickly.
||| `depth` here is the budget; `0` forces a leaf.
export
hexprAt : (depthBudget : Nat) -> Gen HExpr
hexprAt 0 = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 16) ascii
  , Comment <$> string (linear 0 16) ascii
  ]
hexprAt (S k) = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 16) ascii
  , Comment <$> string (linear 0 16) ascii
  , [| Element tagName
              (list (linear 0 3) hattr)
              (list (linear 0 4) (hexprAt k)) |]
  ]

export
hexpr : Gen HExpr
hexpr = hexprAt 3

--------------------------------------------------------------------------------
-- Content-model-aware generators (Phase 2).
--
-- These produce trees that satisfy `Cribrum.Html.Valid.IsValidHtml` by
-- construction: phrasing-only parents only hold phrasing children, flow
-- parents only hold flow/phrasing children, structural parents respect
-- their `OnlyTags` policy.
--------------------------------------------------------------------------------

||| Tags whose content policy is `OnlyCategories [Phrasing]` and which
||| are themselves phrasing content — safe to nest under any phrasing
||| or flow parent.
export
phrasingTagName : Gen String
phrasingTagName = element $ the (Vect _ String)
  [ "span", "em", "strong", "mark", "code", "small", "sub", "sup"
  , "i", "b", "u", "abbr", "cite", "kbd", "samp", "var", "time", "q"
  ]

||| Tags whose content policy is `OnlyCategories [Flow]` and which are
||| themselves flow content — safe to nest under any flow parent.
export
flowTagName : Gen String
flowTagName = element $ the (Vect _ String)
  [ "div", "section", "article", "aside", "nav"
  , "header", "footer", "blockquote", "main"
  ]

||| A tree whose root is phrasing content (text, comment, or a phrasing
||| element holding phrasing children). Safe under any phrasing-or-flow
||| parent.
export
genPhrasingTree : (depthBudget : Nat) -> Gen HExpr
genPhrasingTree 0 = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 8) ascii
  , Comment <$> string (linear 0 8) ascii
  ]
genPhrasingTree (S k) = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 8) ascii
  , Comment <$> string (linear 0 8) ascii
  , [| Element phrasingTagName (pure []) (list (linear 0 3) (genPhrasingTree k)) |]
  ]

||| A tree whose root is flow content. Phrasing trees are accepted at
||| flow positions (phrasing ⊆ flow). Includes block-level constructors
||| (p with phrasing children, ul with li wrappers, etc.) so the
||| generator exercises structural parents.
export
genFlowTree : (depthBudget : Nat) -> Gen HExpr
genFlowTree 0 = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 8) ascii
  , Comment <$> string (linear 0 8) ascii
  ]
genFlowTree (S k) = choice $ the (Vect _ (Gen HExpr))
  [ Text    <$> string (linear 0 8) ascii
  , Comment <$> string (linear 0 8) ascii
  , genPhrasingTree (S k)
  , [| Element flowTagName    (pure []) (list (linear 0 3) (genFlowTree k))    |]
  , [| Element (pure "p")     (pure []) (list (linear 0 3) (genPhrasingTree k)) |]
  , [| Element (pure "h1")    (pure []) (list (linear 0 3) (genPhrasingTree k)) |]
  , [| Element (pure "h2")    (pure []) (list (linear 0 3) (genPhrasingTree k)) |]
  , [| Element (pure "ul")    (pure [])
         (list (linear 0 3)
            [| Element (pure "li") (pure []) (list (linear 0 2) (genFlowTree k)) |]) |]
  , [| Element (pure "ol")    (pure [])
         (list (linear 0 3)
            [| Element (pure "li") (pure []) (list (linear 0 2) (genFlowTree k)) |]) |]
  ]

||| A document-level tree (a flow tree). Alias for `genFlowTree`.
export
genValidTree : (depthBudget : Nat) -> Gen HExpr
genValidTree = genFlowTree
