||| The HExpr — Cribrum's single intermediate representation.
|||
||| Per `plan.md` §Governing principle: there is exactly one datatype. Its tag
||| space is the HTML element set; its attribute model is the HTML attribute
||| model (handler-capable). `div`/`span` are two elements among many, never a
||| catch-all. Conformance ("is valid HTML", "is AA-conformant") is a predicate
||| over HExpr, never a separate datatype.
module Cribrum.Node

%default total

||| Attribute payload. `Str` is a literal string value; `Handler` reserves an
||| event hook (event name, opaque callback id) so the type does not need to
||| change when render-time event wiring lands in Phase 5.
public export
data AttrValue : Type where
  Str     : String -> AttrValue
  Handler : (event : String) -> (callbackId : String) -> AttrValue

public export
Eq AttrValue where
  Str a       == Str b       = a == b
  Handler e c == Handler f d = e == f && c == d
  _           == _           = False

||| One HTML attribute on an element.
public export
record HAttr where
  constructor MkHAttr
  name  : String
  value : AttrValue

public export
Eq HAttr where
  (MkHAttr n v) == (MkHAttr m w) = n == m && v == w

||| The IR. `tag` carries the raw HTML element name; whether it is in the
||| permitted set is the job of `IsValidHtml` (Phase 2), not this datatype.
public export
data HExpr : Type where
  Element : (tag : String) -> (attrs : List HAttr) -> (children : List HExpr) -> HExpr
  Text    : (content : String) -> HExpr
  Comment : (content : String) -> HExpr

public export
Eq HExpr where
  Text a         == Text b         = a == b
  Comment a      == Comment b      = a == b
  Element t a cs == Element u b ds =
    t == u && a == b && assert_total (cs == ds)
  _              == _              = False

public export
Show AttrValue where
  show (Str s)         = "Str " ++ show s
  show (Handler e cid) = "Handler " ++ show e ++ " " ++ show cid

public export
Show HAttr where
  show (MkHAttr n v) = "MkHAttr " ++ show n ++ " (" ++ show v ++ ")"

public export
Show HExpr where
  show (Text s)         = "Text " ++ show s
  show (Comment s)      = "Comment " ++ show s
  show (Element t a cs) =
    "Element " ++ show t ++ " " ++ show a ++ " "
      ++ assert_total (show cs)

||| Count of all nodes in the tree, including the root.
public export
size : HExpr -> Nat
size (Text _)            = 1
size (Comment _)         = 1
size (Element _ _ cs)    = S (sum (assert_total (map size cs)))

||| Maximum depth from root to leaf (root alone has depth 1).
public export
depth : HExpr -> Nat
depth (Text _)         = 1
depth (Comment _)      = 1
depth (Element _ _ []) = 1
depth (Element _ _ cs) = S (foldr max 0 (assert_total (map depth cs)))

||| Pre-order traversal: root, then each child's traversal in order.
public export
walk : HExpr -> List HExpr
walk h@(Text _)         = [h]
walk h@(Comment _)      = [h]
walk h@(Element _ _ cs) = h :: assert_total (concatMap walk cs)

||| All descendants (pre-order), excluding the root itself.
public export
descendants : HExpr -> List HExpr
descendants (Text _)         = []
descendants (Comment _)      = []
descendants (Element _ _ cs) = assert_total (concatMap walk cs)
