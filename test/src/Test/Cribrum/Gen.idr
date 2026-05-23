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
