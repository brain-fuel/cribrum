||| Djot surface AST — Phase 1a per `plan.dj`.
|||
||| Faithful, lossless representation of a `.dj` document. Divs/spans/classes
||| live here because that is what Djot is. Downstream consumers never see this
||| tree; elaboration (Phase 1b) is the only legitimate consumer. The HExpr is
||| the IR; this AST exists *solely* so the parser is a correct, ecosystem-
||| compatible Djot parser.
|||
||| Inventory follows `doc/syntax.html` in `jgm/djot` (block + inline + cross-
||| cutting). Every construct enumerated in plan.dj §Phase 1a is representable.
module Cribrum.Djot.Surface

%default total

--------------------------------------------------------------------------------
-- Generic attribute block (the cross-cutting mechanism).
--------------------------------------------------------------------------------

||| Djot's generic attribute block. Applied uniformly to any node: `{#id .class
||| key=value}`. Empty attrs = the construct has no attribute block attached.
public export
record Attrs where
  constructor MkAttrs
  identifier : Maybe String
  classes    : List String
  pairs      : List (String, String)

public export
emptyAttrs : Attrs
emptyAttrs = MkAttrs Nothing [] []

public export
Eq Attrs where
  (MkAttrs a b c) == (MkAttrs x y z) = a == x && b == y && c == z

public export
Show Attrs where
  show (MkAttrs i cs ps) =
    "MkAttrs " ++ show i ++ " " ++ show cs ++ " " ++ show ps

--------------------------------------------------------------------------------
-- Non-recursive enums and link refs.
--------------------------------------------------------------------------------

||| List markers. Djot supports decimal/roman/alpha ordered, dash/asterisk/plus
||| unordered, task list, and definition list. The `start` metadata for ordered
||| lists is carried on `ListBlock`.
public export
data ListStyle
  = OrderedDecimal
  | OrderedRomanLower
  | OrderedRomanUpper
  | OrderedAlphaLower
  | OrderedAlphaUpper
  | UnorderedDash
  | UnorderedAsterisk
  | UnorderedPlus
  | TaskList
  | Definition

public export
Eq ListStyle where
  OrderedDecimal    == OrderedDecimal    = True
  OrderedRomanLower == OrderedRomanLower = True
  OrderedRomanUpper == OrderedRomanUpper = True
  OrderedAlphaLower == OrderedAlphaLower = True
  OrderedAlphaUpper == OrderedAlphaUpper = True
  UnorderedDash     == UnorderedDash     = True
  UnorderedAsterisk == UnorderedAsterisk = True
  UnorderedPlus     == UnorderedPlus     = True
  TaskList          == TaskList          = True
  Definition        == Definition        = True
  _                 == _                 = False

public export
Show ListStyle where
  show OrderedDecimal    = "OrderedDecimal"
  show OrderedRomanLower = "OrderedRomanLower"
  show OrderedRomanUpper = "OrderedRomanUpper"
  show OrderedAlphaLower = "OrderedAlphaLower"
  show OrderedAlphaUpper = "OrderedAlphaUpper"
  show UnorderedDash     = "UnorderedDash"
  show UnorderedAsterisk = "UnorderedAsterisk"
  show UnorderedPlus     = "UnorderedPlus"
  show TaskList          = "TaskList"
  show Definition        = "Definition"

||| Pipe-table column alignment.
public export
data Align = AlignNone | AlignLeft | AlignRight | AlignCenter

public export
Eq Align where
  AlignNone   == AlignNone   = True
  AlignLeft   == AlignLeft   = True
  AlignRight  == AlignRight  = True
  AlignCenter == AlignCenter = True
  _           == _           = False

public export
Show Align where
  show AlignNone   = "AlignNone"
  show AlignLeft   = "AlignLeft"
  show AlignRight  = "AlignRight"
  show AlignCenter = "AlignCenter"

||| Smart-punctuation forms emitted by Djot from `--`, `...`, `'`, `"`.
public export
data SmartPunct
  = LDQuote | RDQuote | LSQuote | RSQuote | EnDash | EmDash | Ellipsis

public export
Eq SmartPunct where
  LDQuote  == LDQuote  = True
  RDQuote  == RDQuote  = True
  LSQuote  == LSQuote  = True
  RSQuote  == RSQuote  = True
  EnDash   == EnDash   = True
  EmDash   == EmDash   = True
  Ellipsis == Ellipsis = True
  _        == _        = False

public export
Show SmartPunct where
  show LDQuote  = "LDQuote"
  show RDQuote  = "RDQuote"
  show LSQuote  = "LSQuote"
  show RSQuote  = "RSQuote"
  show EnDash   = "EnDash"
  show EmDash   = "EmDash"
  show Ellipsis = "Ellipsis"

||| Link/image references. Djot distinguishes inline, reference, and autolink.
public export
data LinkRef : Type where
  LinkInline    : (url : String) -> (title : Maybe String) -> LinkRef
  LinkReference : (label : String) -> LinkRef
  LinkAuto      : (url : String) -> LinkRef

public export
Eq LinkRef where
  LinkInline u t    == LinkInline v s    = u == v && t == s
  LinkReference l   == LinkReference m   = l == m
  LinkAuto u        == LinkAuto v        = u == v
  _                 == _                 = False

public export
Show LinkRef where
  show (LinkInline u t)  = "LinkInline " ++ show u ++ " " ++ show t
  show (LinkReference l) = "LinkReference " ++ show l
  show (LinkAuto u)      = "LinkAuto " ++ show u

--------------------------------------------------------------------------------
-- Inline tree (self-recursive; no Block dependency).
--------------------------------------------------------------------------------

public export
data Inline : Type where
  InlText        : String -> Inline
  InlSoftBreak   : Inline
  InlHardBreak   : Inline
  InlComment     : String -> Inline
  InlEmph        : List Inline -> Inline
  InlStrong      : List Inline -> Inline
  InlHighlight   : List Inline -> Inline
  InlSuper       : List Inline -> Inline
  InlSub         : List Inline -> Inline
  InlInsert      : List Inline -> Inline
  InlDelete      : List Inline -> Inline
  InlVerbatim    : Attrs -> String -> Inline
  InlLink        : Attrs -> LinkRef -> List Inline -> Inline
  InlImage       : Attrs -> LinkRef -> List Inline -> Inline
  InlMath        : (isDisplay : Bool) -> String -> Inline
  InlFootnoteRef : (label : String) -> Inline
  InlSymbol      : (name : String) -> Inline
  InlRaw         : (format : String) -> String -> Inline
  InlSpan        : Attrs -> List Inline -> Inline
  InlSmart       : SmartPunct -> Inline

public export
Eq Inline where
  InlText a         == InlText b         = a == b
  InlSoftBreak      == InlSoftBreak      = True
  InlHardBreak      == InlHardBreak      = True
  InlComment a      == InlComment b      = a == b
  InlEmph a         == InlEmph b         = assert_total (a == b)
  InlStrong a       == InlStrong b       = assert_total (a == b)
  InlHighlight a    == InlHighlight b    = assert_total (a == b)
  InlSuper a        == InlSuper b        = assert_total (a == b)
  InlSub a          == InlSub b          = assert_total (a == b)
  InlInsert a       == InlInsert b       = assert_total (a == b)
  InlDelete a       == InlDelete b       = assert_total (a == b)
  InlVerbatim a s   == InlVerbatim b t   = a == b && s == t
  InlLink a r xs    == InlLink b s ys    = a == b && r == s && assert_total (xs == ys)
  InlImage a r xs   == InlImage b s ys   = a == b && r == s && assert_total (xs == ys)
  InlMath d s       == InlMath e t       = d == e && s == t
  InlFootnoteRef a  == InlFootnoteRef b  = a == b
  InlSymbol a       == InlSymbol b       = a == b
  InlRaw f s        == InlRaw g t        = f == g && s == t
  InlSpan a xs      == InlSpan b ys      = a == b && assert_total (xs == ys)
  InlSmart a        == InlSmart b        = a == b
  _                 == _                 = False

public export
Show Inline where
  show (InlText s)        = "InlText " ++ show s
  show InlSoftBreak       = "InlSoftBreak"
  show InlHardBreak       = "InlHardBreak"
  show (InlComment s)     = "InlComment " ++ show s
  show (InlEmph xs)       = "InlEmph "      ++ assert_total (show xs)
  show (InlStrong xs)     = "InlStrong "    ++ assert_total (show xs)
  show (InlHighlight xs)  = "InlHighlight " ++ assert_total (show xs)
  show (InlSuper xs)      = "InlSuper "     ++ assert_total (show xs)
  show (InlSub xs)        = "InlSub "       ++ assert_total (show xs)
  show (InlInsert xs)     = "InlInsert "    ++ assert_total (show xs)
  show (InlDelete xs)     = "InlDelete "    ++ assert_total (show xs)
  show (InlVerbatim a s)  = "InlVerbatim "  ++ show a ++ " " ++ show s
  show (InlLink a r xs)   =
    "InlLink "  ++ show a ++ " " ++ show r ++ " " ++ assert_total (show xs)
  show (InlImage a r xs)  =
    "InlImage " ++ show a ++ " " ++ show r ++ " " ++ assert_total (show xs)
  show (InlMath d s)      = "InlMath "      ++ show d ++ " " ++ show s
  show (InlFootnoteRef l) = "InlFootnoteRef " ++ show l
  show (InlSymbol n)      = "InlSymbol "    ++ show n
  show (InlRaw f s)       = "InlRaw "       ++ show f ++ " " ++ show s
  show (InlSpan a xs)     = "InlSpan "      ++ show a ++ " " ++ assert_total (show xs)
  show (InlSmart sp)      = "InlSmart "     ++ show sp

--------------------------------------------------------------------------------
-- Block tree (mutual with ListItem + TableRow + TableCell).
--------------------------------------------------------------------------------

mutual
  public export
  data Block : Type where
    Paragraph     : Attrs -> List Inline -> Block
    Heading       : Attrs -> (level : Nat) -> List Inline -> Block
    BlockQuote    : Attrs -> List Block -> Block
    ListBlock     : Attrs -> ListStyle -> (start : Maybe Nat)
                          -> (tight : Bool) -> List ListItem -> Block
    CodeBlock     : Attrs -> (info : String) -> (body : String) -> Block
    ThematicBreak : Attrs -> Block
    RawBlock      : (format : String) -> (body : String) -> Block
    Div           : Attrs -> List Block -> Block
    Table         : Attrs -> (caption : Maybe (List Inline))
                          -> List TableRow -> Block
    RefDef        : (label : String) -> (dest : String)
                                     -> (title : Maybe String) -> Block
    FootnoteDef   : Attrs -> (label : String) -> List Block -> Block

  ||| List item. `checked` is `Just b` only for `TaskList`; `term` only for
  ||| `Definition`. The parent `ListBlock`'s `ListStyle` is the source of truth.
  public export
  record ListItem where
    constructor MkLI
    attrs   : Attrs
    checked : Maybe Bool
    term    : Maybe (List Inline)
    content : List Block

  public export
  record TableRow where
    constructor MkRow
    isHeader : Bool
    cells    : List TableCell

  public export
  record TableCell where
    constructor MkCell
    align   : Align
    content : List Inline

mutual
  public export
  Eq Block where
    Paragraph a xs         == Paragraph b ys         =
      a == b && xs == ys
    Heading a l xs         == Heading b m ys         =
      a == b && l == m && xs == ys
    BlockQuote a xs        == BlockQuote b ys        =
      a == b && assert_total (xs == ys)
    ListBlock a s st t xs  == ListBlock b r su u ys  =
      a == b && s == r && st == su && t == u
        && assert_total (xs == ys)
    CodeBlock a i b1       == CodeBlock c j b2       =
      a == c && i == j && b1 == b2
    ThematicBreak a        == ThematicBreak b        = a == b
    RawBlock f s           == RawBlock g t           = f == g && s == t
    Div a xs               == Div b ys               =
      a == b && assert_total (xs == ys)
    Table a c rs           == Table b d ss           =
      a == b && c == d && assert_total (rs == ss)
    RefDef l d t           == RefDef m e u           =
      l == m && d == e && t == u
    FootnoteDef a l xs     == FootnoteDef b m ys     =
      a == b && l == m && assert_total (xs == ys)
    _                      == _                      = False

  public export
  Eq ListItem where
    (MkLI a c t xs) == (MkLI b d u ys) =
      a == b && c == d && t == u && assert_total (xs == ys)

  public export
  Eq TableRow where
    (MkRow h cs) == (MkRow k ds) = h == k && cs == ds

  public export
  Eq TableCell where
    (MkCell a xs) == (MkCell b ys) = a == b && xs == ys

mutual
  public export
  Show Block where
    show (Paragraph a xs)        = "Paragraph " ++ show a ++ " " ++ show xs
    show (Heading a l xs)        =
      "Heading " ++ show a ++ " " ++ show l ++ " " ++ show xs
    show (BlockQuote a xs)       =
      "BlockQuote " ++ show a ++ " " ++ assert_total (show xs)
    show (ListBlock a s st t xs) =
      "ListBlock " ++ show a ++ " " ++ show s ++ " " ++ show st
        ++ " " ++ show t ++ " " ++ assert_total (show xs)
    show (CodeBlock a i b)       =
      "CodeBlock " ++ show a ++ " " ++ show i ++ " " ++ show b
    show (ThematicBreak a)       = "ThematicBreak " ++ show a
    show (RawBlock f s)          = "RawBlock " ++ show f ++ " " ++ show s
    show (Div a xs)              =
      "Div " ++ show a ++ " " ++ assert_total (show xs)
    show (Table a c rs)          =
      "Table " ++ show a ++ " " ++ show c ++ " " ++ assert_total (show rs)
    show (RefDef l d t)          =
      "RefDef " ++ show l ++ " " ++ show d ++ " " ++ show t
    show (FootnoteDef a l xs)    =
      "FootnoteDef " ++ show a ++ " " ++ show l
        ++ " " ++ assert_total (show xs)

  public export
  Show ListItem where
    show (MkLI a c t xs) =
      "MkLI " ++ show a ++ " " ++ show c ++ " " ++ show t
        ++ " " ++ assert_total (show xs)

  public export
  Show TableRow where
    show (MkRow h cs) = "MkRow " ++ show h ++ " " ++ show cs

  public export
  Show TableCell where
    show (MkCell a xs) = "MkCell " ++ show a ++ " " ++ show xs

--------------------------------------------------------------------------------
-- Document.
--------------------------------------------------------------------------------

||| Top-level document.
public export
record Doc where
  constructor MkDoc
  blocks : List Block

public export
Eq Doc where
  (MkDoc xs) == (MkDoc ys) = xs == ys

public export
Show Doc where
  show (MkDoc xs) = "MkDoc " ++ show xs

--------------------------------------------------------------------------------
-- Traversal: block & inline counts.
--------------------------------------------------------------------------------

mutual
  ||| Count of block nodes in the document, including nested blocks.
  public export
  countBlocks : List Block -> Nat
  countBlocks []        = 0
  countBlocks (b :: bs) = blockSize b + countBlocks bs

  public export
  blockSize : Block -> Nat
  blockSize (Paragraph _ _)         = 1
  blockSize (Heading _ _ _)         = 1
  blockSize (BlockQuote _ bs)       = S (assert_total (countBlocks bs))
  blockSize (ListBlock _ _ _ _ lis) = S (assert_total (sumItems lis))
  blockSize (CodeBlock _ _ _)       = 1
  blockSize (ThematicBreak _)       = 1
  blockSize (RawBlock _ _)          = 1
  blockSize (Div _ bs)              = S (assert_total (countBlocks bs))
  blockSize (Table _ _ _)           = 1
  blockSize (RefDef _ _ _)          = 1
  blockSize (FootnoteDef _ _ bs)    = S (assert_total (countBlocks bs))

  public export
  sumItems : List ListItem -> Nat
  sumItems []        = 0
  sumItems (i :: is) =
    assert_total (countBlocks (content i)) + sumItems is

||| Sum of inline-tree sizes inside `xs`.
public export
inlineSize : Inline -> Nat
inlineSize (InlText _)       = 1
inlineSize InlSoftBreak      = 1
inlineSize InlHardBreak      = 1
inlineSize (InlComment _)    = 1
inlineSize (InlEmph xs)      = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlStrong xs)    = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlHighlight xs) = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlSuper xs)     = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlSub xs)       = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlInsert xs)    = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlDelete xs)    = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlVerbatim _ _) = 1
inlineSize (InlLink _ _ xs)  = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlImage _ _ xs) = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlMath _ _)     = 1
inlineSize (InlFootnoteRef _)= 1
inlineSize (InlSymbol _)     = 1
inlineSize (InlRaw _ _)      = 1
inlineSize (InlSpan _ xs)    = S (sum (assert_total (map inlineSize xs)))
inlineSize (InlSmart _)      = 1
