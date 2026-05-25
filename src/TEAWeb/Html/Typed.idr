||| TEAWeb.Html.Typed — Phase 2 post-ingestion smart constructors.
|||
||| Per `plan.dj` §Phase T1 (post-Phase-2 follow-on) and §P4.1: the
||| view-builder's content-model gate moves from `viewSafe`'s dynamic
||| `decideHtmlLocated` to compile-time auto-discharge.
|||
||| Design (per the typed-tag wrapper pattern):
|||
||| - Each element-producing typed smart constructor returns
|||   `TypedView <tag> msg`; the runtime `View msg` lives inside, the
|||   tag is a type-level string literal.
||| - Each child position is a `Child <parent> msg`: a wrapper that
|||   carries an auto-implicit witness
|||   `So (isTagAllowedIn parent <childTag>)`.
||| - `isTagAllowedIn` is implemented as a tag-pattern-matched
|||   policy (parent → child → Bool). String-literal pattern matches
|||   reduce at the elaborator, so the auto witness becomes `Oh`
|||   without forcing Idris to unfold the 114-element runtime catalog.
||| - Drift between the hand-coded policy and the catalog is gated by
|||   the test suite (`pddt_typed_predicate_matches_catalog`): every
|||   typed (parent, child) pair must agree with
|||   `Cribrum.Html.Valid.childAllowedBool`. The pattern is identical
|||   to the I2 invariant — TS-side catalog is authoritative, Idris
|||   side carries a fast specialization with a propositional bridge.
|||
||| The dynamic `viewSafe` gate in `TEAWeb.Html` stays as a runtime
||| safety net for views produced by means other than these typed
||| constructors (manual `MkView`, FFI bridges, future Phase-1b
||| elaboration output).
module TEAWeb.Html.Typed

import Data.List
import Data.So
import Cribrum.Node
import Cribrum.Html.Category
import Cribrum.Html.Model
import Cribrum.Html.Valid
import TEAWeb.Html

%default total

--------------------------------------------------------------------------------
-- Category membership over the tag alphabet. The Phrasing / Flow / Void
-- partitions cover the parents we expose typed constructors for.
--------------------------------------------------------------------------------

||| `True` iff `tag` is HTML-spec phrasing content. Source: every
||| catalog entry with `Phrasing` in its category list (mirrored here so
||| the elaborator can reduce the check directly).
public export
isPhrasingTag : String -> Bool
isPhrasingTag "a"        = True
isPhrasingTag "abbr"     = True
isPhrasingTag "area"     = True
isPhrasingTag "audio"    = True
isPhrasingTag "b"        = True
isPhrasingTag "bdi"      = True
isPhrasingTag "bdo"      = True
isPhrasingTag "br"       = True
isPhrasingTag "button"   = True
isPhrasingTag "canvas"   = True
isPhrasingTag "cite"     = True
isPhrasingTag "code"     = True
isPhrasingTag "data"     = True
isPhrasingTag "datalist" = True
isPhrasingTag "del"      = True
isPhrasingTag "dfn"      = True
isPhrasingTag "em"       = True
isPhrasingTag "embed"    = True
isPhrasingTag "i"        = True
isPhrasingTag "iframe"   = True
isPhrasingTag "img"      = True
isPhrasingTag "input"    = True
isPhrasingTag "ins"      = True
isPhrasingTag "kbd"      = True
isPhrasingTag "label"    = True
isPhrasingTag "map"      = True
isPhrasingTag "mark"     = True
isPhrasingTag "math"     = True
isPhrasingTag "meter"    = True
isPhrasingTag "noscript" = True
isPhrasingTag "object"   = True
isPhrasingTag "output"   = True
isPhrasingTag "picture"  = True
isPhrasingTag "progress" = True
isPhrasingTag "q"        = True
isPhrasingTag "ruby"     = True
isPhrasingTag "s"        = True
isPhrasingTag "samp"     = True
isPhrasingTag "script"   = True
isPhrasingTag "select"   = True
isPhrasingTag "slot"     = True
isPhrasingTag "small"    = True
isPhrasingTag "span"     = True
isPhrasingTag "strong"   = True
isPhrasingTag "sub"      = True
isPhrasingTag "sup"      = True
isPhrasingTag "svg"      = True
isPhrasingTag "template" = True
isPhrasingTag "textarea" = True
isPhrasingTag "time"     = True
isPhrasingTag "u"        = True
isPhrasingTag "var"      = True
isPhrasingTag "video"    = True
isPhrasingTag "wbr"      = True
isPhrasingTag _          = False

||| `True` iff `tag` is HTML-spec flow content. Flow is a superset of
||| Phrasing plus block-level elements.
public export
isFlowTag : String -> Bool
isFlowTag t =
       isPhrasingTag t
    || t == "address"  || t == "article"  || t == "aside"
    || t == "blockquote" || t == "details" || t == "dialog"
    || t == "div"      || t == "dl"       || t == "fieldset"
    || t == "figure"   || t == "footer"   || t == "form"
    || t == "h1"       || t == "h2"       || t == "h3"
    || t == "h4"       || t == "h5"       || t == "h6"
    || t == "header"   || t == "hgroup"   || t == "hr"
    || t == "main"     || t == "menu"     || t == "nav"
    || t == "ol"       || t == "p"        || t == "pre"
    || t == "search"   || t == "section"  || t == "table"
    || t == "ul"

--------------------------------------------------------------------------------
-- Tag-keyed permission predicates. Parent-first dispatch keeps every
-- branch a string-literal pattern match the elaborator reduces eagerly.
--------------------------------------------------------------------------------

||| `True` iff a child element with tag `child` is permitted directly
||| inside `parent`, per the catalog.
public export
isTagAllowedIn : (parent : String) -> (child : String) -> Bool
-- Sectioning / Flow parents → flow-content children.
isTagAllowedIn "div"        c = isFlowTag c
isTagAllowedIn "section"    c = isFlowTag c
isTagAllowedIn "article"    c = isFlowTag c
isTagAllowedIn "aside"      c = isFlowTag c
isTagAllowedIn "nav"        c = isFlowTag c
isTagAllowedIn "main"       c = isFlowTag c
isTagAllowedIn "header"     c = isFlowTag c
isTagAllowedIn "footer"     c = isFlowTag c
isTagAllowedIn "blockquote" c = isFlowTag c
isTagAllowedIn "form"       c = isFlowTag c
isTagAllowedIn "li"         c = isFlowTag c
isTagAllowedIn "figcaption" c = isFlowTag c
-- Phrasing parents → phrasing-content children.
isTagAllowedIn "p"          c = isPhrasingTag c
isTagAllowedIn "h1"         c = isPhrasingTag c
isTagAllowedIn "h2"         c = isPhrasingTag c
isTagAllowedIn "h3"         c = isPhrasingTag c
isTagAllowedIn "h4"         c = isPhrasingTag c
isTagAllowedIn "h5"         c = isPhrasingTag c
isTagAllowedIn "h6"         c = isPhrasingTag c
isTagAllowedIn "span"       c = isPhrasingTag c
isTagAllowedIn "em"         c = isPhrasingTag c
isTagAllowedIn "strong"     c = isPhrasingTag c
isTagAllowedIn "code"       c = isPhrasingTag c
isTagAllowedIn "pre"        c = isPhrasingTag c
isTagAllowedIn "button"     c = isPhrasingTag c
isTagAllowedIn "label"      c = isPhrasingTag c
-- Restricted-tag-set parents.
isTagAllowedIn "ul" "li"       = True
isTagAllowedIn "ul" "script"   = True
isTagAllowedIn "ul" "template" = True
isTagAllowedIn "ul" _           = False
isTagAllowedIn "ol" "li"       = True
isTagAllowedIn "ol" "script"   = True
isTagAllowedIn "ol" "template" = True
isTagAllowedIn "ol" _           = False
-- Transparent / unspecified parents: accept anything.
isTagAllowedIn "a"        _ = True
isTagAllowedIn "figure"   _ = True
-- Void elements: no children at all.
isTagAllowedIn "br"       _ = False
isTagAllowedIn "hr"       _ = False
isTagAllowedIn "img"      _ = False
isTagAllowedIn "input"    _ = False
-- Conservative default for any parent we do not yet expose typed.
isTagAllowedIn _          _ = False

||| `True` iff `parent`'s content model accepts raw `Text` nodes.
public export
allowsText : (parent : String) -> Bool
-- Phrasing-policy parents accept text (text is phrasing).
allowsText "p"          = True
allowsText "h1"         = True
allowsText "h2"         = True
allowsText "h3"         = True
allowsText "h4"         = True
allowsText "h5"         = True
allowsText "h6"         = True
allowsText "span"       = True
allowsText "em"         = True
allowsText "strong"     = True
allowsText "code"       = True
allowsText "pre"        = True
allowsText "button"     = True
allowsText "label"      = True
allowsText "figcaption" = True
-- Flow-policy parents accept text (phrasing ⊆ flow).
allowsText "div"        = True
allowsText "section"    = True
allowsText "article"    = True
allowsText "aside"      = True
allowsText "nav"        = True
allowsText "main"       = True
allowsText "header"     = True
allowsText "footer"     = True
allowsText "blockquote" = True
allowsText "form"       = True
allowsText "li"         = True
-- Transparent / unspecified.
allowsText "a"          = True
allowsText "figure"     = True
-- All others (ul/ol with OnlyTags _ False, void elements): no.
allowsText _            = False

||| `True` iff `parent` accepts HTML comments. Every non-void element
||| in the catalog admits comments.
public export
allowsComment : (parent : String) -> Bool
allowsComment "br"    = False
allowsComment "hr"    = False
allowsComment "img"   = False
allowsComment "input" = False
allowsComment _       = True

--------------------------------------------------------------------------------
-- Phantom-typed view wrapper.
--------------------------------------------------------------------------------

||| A `View msg` whose root tag is statically known to be `tag`.
public export
record TypedView (tag : String) (msg : Type) where
  constructor MkTypedView
  view : View msg

||| A permitted child position under an element with tag `parent`.
public export
data Child : (parent : String) -> (msg : Type) -> Type where
  ElementChild :  {0 tag : String}
              -> {auto ok : So (isTagAllowedIn parent tag)}
              -> TypedView tag msg
              -> Child parent msg
  TextChild    :  {auto ok : So (allowsText parent)}
              -> View msg
              -> Child parent msg
  CommentChild :  {auto ok : So (allowsComment parent)}
              -> View msg
              -> Child parent msg

||| Project a `Child parent msg` back to its underlying `View msg`.
childView : Child parent msg -> View msg
childView (ElementChild tv) = view tv
childView (TextChild v)     = v
childView (CommentChild v)  = v

--------------------------------------------------------------------------------
-- Child-position coercions.
--------------------------------------------------------------------------------

||| Lift a typed element view into a child position under `parent`.
||| Auto-resolves to `Oh` iff the catalog says yes.
public export
c_ :  {0 parent : String} -> {0 tag : String}
   -> {auto ok : So (isTagAllowedIn parent tag)}
   -> TypedView tag msg -> Child parent msg
c_ tv = ElementChild {ok} tv

||| Plain text child. Discharged iff the parent's content model
||| accepts text nodes.
public export
tx_ :  {0 parent : String}
    -> {auto ok : So (allowsText parent)}
    -> String -> Child parent msg
tx_ {ok} s = TextChild {ok} (text_ s)

||| Comment child. Discharged iff the parent's content model accepts
||| comments (true for every non-void element).
public export
cm_ :  {0 parent : String}
    -> {auto ok : So (allowsComment parent)}
    -> String -> Child parent msg
cm_ {ok} s = CommentChild {ok} (comment_ s)

--------------------------------------------------------------------------------
-- Generic typed element constructor.
--------------------------------------------------------------------------------

||| Build an element with tag `parent` from typed children. The
||| children list is collapsed back to `List (View msg)` for
||| `mkElement`; the compile-time witnesses guarantee every child is
||| catalog-permitted.
public export
typedElement_ :  (parent : String) -> List (Attr msg) -> List (Child parent msg)
              -> TypedView parent msg
typedElement_ parent attrs cs =
  MkTypedView (mkElement parent attrs (map childView cs))

--------------------------------------------------------------------------------
-- Per-element specializations.
--------------------------------------------------------------------------------

public export
divT_ : List (Attr msg) -> List (Child "div" msg) -> TypedView "div" msg
divT_ = typedElement_ "div"

public export
spanT_ : List (Attr msg) -> List (Child "span" msg) -> TypedView "span" msg
spanT_ = typedElement_ "span"

public export
pT_ : List (Attr msg) -> List (Child "p" msg) -> TypedView "p" msg
pT_ = typedElement_ "p"

public export
h1T_ : List (Attr msg) -> List (Child "h1" msg) -> TypedView "h1" msg
h1T_ = typedElement_ "h1"

public export
h2T_ : List (Attr msg) -> List (Child "h2" msg) -> TypedView "h2" msg
h2T_ = typedElement_ "h2"

public export
h3T_ : List (Attr msg) -> List (Child "h3" msg) -> TypedView "h3" msg
h3T_ = typedElement_ "h3"

public export
h4T_ : List (Attr msg) -> List (Child "h4" msg) -> TypedView "h4" msg
h4T_ = typedElement_ "h4"

public export
h5T_ : List (Attr msg) -> List (Child "h5" msg) -> TypedView "h5" msg
h5T_ = typedElement_ "h5"

public export
h6T_ : List (Attr msg) -> List (Child "h6" msg) -> TypedView "h6" msg
h6T_ = typedElement_ "h6"

public export
aT_ : List (Attr msg) -> List (Child "a" msg) -> TypedView "a" msg
aT_ = typedElement_ "a"

public export
buttonT_ : List (Attr msg) -> List (Child "button" msg) -> TypedView "button" msg
buttonT_ = typedElement_ "button"

public export
formT_ : List (Attr msg) -> List (Child "form" msg) -> TypedView "form" msg
formT_ = typedElement_ "form"

public export
ulT_ : List (Attr msg) -> List (Child "ul" msg) -> TypedView "ul" msg
ulT_ = typedElement_ "ul"

public export
olT_ : List (Attr msg) -> List (Child "ol" msg) -> TypedView "ol" msg
olT_ = typedElement_ "ol"

public export
liT_ : List (Attr msg) -> List (Child "li" msg) -> TypedView "li" msg
liT_ = typedElement_ "li"

public export
sectionT_ : List (Attr msg) -> List (Child "section" msg) -> TypedView "section" msg
sectionT_ = typedElement_ "section"

public export
articleT_ : List (Attr msg) -> List (Child "article" msg) -> TypedView "article" msg
articleT_ = typedElement_ "article"

public export
asideT_ : List (Attr msg) -> List (Child "aside" msg) -> TypedView "aside" msg
asideT_ = typedElement_ "aside"

public export
mainT_ : List (Attr msg) -> List (Child "main" msg) -> TypedView "main" msg
mainT_ = typedElement_ "main"

public export
navT_ : List (Attr msg) -> List (Child "nav" msg) -> TypedView "nav" msg
navT_ = typedElement_ "nav"

public export
headerT_ : List (Attr msg) -> List (Child "header" msg) -> TypedView "header" msg
headerT_ = typedElement_ "header"

public export
footerT_ : List (Attr msg) -> List (Child "footer" msg) -> TypedView "footer" msg
footerT_ = typedElement_ "footer"

public export
figureT_ : List (Attr msg) -> List (Child "figure" msg) -> TypedView "figure" msg
figureT_ = typedElement_ "figure"

public export
figcaptionT_ : List (Attr msg) -> List (Child "figcaption" msg) -> TypedView "figcaption" msg
figcaptionT_ = typedElement_ "figcaption"

public export
labelT_ : List (Attr msg) -> List (Child "label" msg) -> TypedView "label" msg
labelT_ = typedElement_ "label"

public export
strongT_ : List (Attr msg) -> List (Child "strong" msg) -> TypedView "strong" msg
strongT_ = typedElement_ "strong"

public export
emT_ : List (Attr msg) -> List (Child "em" msg) -> TypedView "em" msg
emT_ = typedElement_ "em"

public export
codeT_ : List (Attr msg) -> List (Child "code" msg) -> TypedView "code" msg
codeT_ = typedElement_ "code"

public export
preT_ : List (Attr msg) -> List (Child "pre" msg) -> TypedView "pre" msg
preT_ = typedElement_ "pre"

public export
blockquoteT_ : List (Attr msg) -> List (Child "blockquote" msg) -> TypedView "blockquote" msg
blockquoteT_ = typedElement_ "blockquote"

--------------------------------------------------------------------------------
-- Void-element typed wrappers — no children to type-check.
--------------------------------------------------------------------------------

public export
brT_ : List (Attr msg) -> TypedView "br" msg
brT_ attrs = MkTypedView (mkElement "br" attrs [])

public export
hrT_ : List (Attr msg) -> TypedView "hr" msg
hrT_ attrs = MkTypedView (mkElement "hr" attrs [])

public export
imgT_ : List (Attr msg) -> TypedView "img" msg
imgT_ attrs = MkTypedView (mkElement "img" attrs [])

public export
inputT_ : List (Attr msg) -> TypedView "input" msg
inputT_ attrs = MkTypedView (mkElement "input" attrs [])

--------------------------------------------------------------------------------
-- Bridge back to the untyped TEAWeb.Html layer.
--------------------------------------------------------------------------------

public export
unTyped : TypedView tag msg -> View msg
unTyped tv = view tv
