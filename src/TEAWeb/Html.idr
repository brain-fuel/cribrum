||| TEAWeb.Html — view-builder smart constructors (Phase T1).
|||
||| Smart constructors return `View msg`, which carries both the
||| underlying `HExpr` (Cribrum's single IR) and the `HandlerTable` of
||| msg-producing callbacks. The IR is *not* widened: `HExpr`'s
||| `Handler` attribute already exists to carry the callback id; the
||| msg-producing closure lives in the `HandlerTable` and is registered
||| with the runtime at mount time.
|||
||| Children are `View msg`, never a parallel `View` tree — once
||| unwrapped at the end of `view`, what mounts is a plain `HExpr`. The
||| single-type invariant from plan.dj §Governing principle survives.
|||
||| Pre-Phase-2 (today): smart constructors emit unchecked tag/children;
||| `viewSafe` runs the resulting tree through `decideHtml` and returns a
||| located rejection if anything is malformed. Post-Phase-2, the smart
||| constructors will be typed by the per-element content model so this
||| dynamic gate becomes unnecessary (children's types refuse illegal
||| placement at compile time).
module TEAWeb.Html

import Data.List
import Cribrum.Node
import Cribrum.Html.Valid

%default total

--------------------------------------------------------------------------------
-- Event: opaque handle to a DOM Event under the JS backend.
--------------------------------------------------------------------------------

||| Opaque DOM `Event` handle. Behaves like `Cribrum.Render.Dom.DomNode`
||| — type-checks under chez, only constructed by the JS-backend dispatch
||| shim at runtime.
public export
data Event : Type where [external]

--------------------------------------------------------------------------------
-- Attribute type wrapping HAttr with optional msg-producing callback.
--------------------------------------------------------------------------------

||| A view-builder attribute. `Plain` is a normal HTML attribute; `On`
||| registers an event handler. The Handle case stores both the HExpr-
||| level `HAttr` (so the wire format `data-on-<event>="<cbId>"` lands in
||| the IR) and the msg-producing closure (so the runtime can resolve a
||| dispatched event back to a `msg`).
public export
data Attr : Type -> Type where
  Plain  : HAttr -> Attr msg
  On     : (event : String)
        -> (callbackId : String)
        -> (Event -> msg)
        -> Attr msg

--------------------------------------------------------------------------------
-- View: HExpr + handler table.
--------------------------------------------------------------------------------

||| A view-builder result. The `tree` is plain HExpr (rendered to HTML/
||| DOM by Cribrum); `handlers` are the msg-producing callbacks
||| registered against their callback ids. Mount registers the handlers
||| with the runtime's dispatch closure and then mounts the tree.
public export
record View (msg : Type) where
  constructor MkView
  tree     : HExpr
  handlers : List (String, Event -> msg)

--------------------------------------------------------------------------------
-- Attribute → (HAttr, optional handler) split.
--------------------------------------------------------------------------------

attrToHAttr : Attr msg -> HAttr
attrToHAttr (Plain ha)        = ha
attrToHAttr (On ev cbId _)    = MkHAttr ("data-on-" ++ ev) (Handler ev cbId)

attrToHandler : Attr msg -> Maybe (String, Event -> msg)
attrToHandler (Plain _)         = Nothing
attrToHandler (On _ cbId f)     = Just (cbId, f)

splitAttrs : List (Attr msg) -> (List HAttr, List (String, Event -> msg))
splitAttrs attrs =
  ( map attrToHAttr attrs
  , mapMaybe attrToHandler attrs
  )

--------------------------------------------------------------------------------
-- Core element constructor.
--------------------------------------------------------------------------------

||| Build a `View msg` for an HTML element. The handler table accumulates
||| handlers from both this element's attrs and recursively from
||| children's views.
public export
mkElement : (tag : String) -> List (Attr msg) -> List (View msg) -> View msg
mkElement tag attrs children =
  let (hattrs, attrHandlers) = splitAttrs attrs
      kidTrees    = map tree children
      kidHandlers = concatMap handlers children
   in MkView
        (Element tag hattrs kidTrees)
        (attrHandlers ++ kidHandlers)

--------------------------------------------------------------------------------
-- Leaf constructors.
--------------------------------------------------------------------------------

||| Plain text node. Inherits no handlers.
public export
text_ : String -> View msg
text_ s = MkView (Text s) []

||| HTML comment. Renders as `<!-- ... -->`; no handlers.
public export
comment_ : String -> View msg
comment_ s = MkView (Comment s) []

--------------------------------------------------------------------------------
-- Element shorthands (one per common HTML element).
--------------------------------------------------------------------------------

public export
div_      : List (Attr msg) -> List (View msg) -> View msg
div_      = mkElement "div"

public export
span_     : List (Attr msg) -> List (View msg) -> View msg
span_     = mkElement "span"

public export
p_        : List (Attr msg) -> List (View msg) -> View msg
p_        = mkElement "p"

public export
h1_       : List (Attr msg) -> List (View msg) -> View msg
h1_       = mkElement "h1"

public export
h2_       : List (Attr msg) -> List (View msg) -> View msg
h2_       = mkElement "h2"

public export
h3_       : List (Attr msg) -> List (View msg) -> View msg
h3_       = mkElement "h3"

public export
h4_       : List (Attr msg) -> List (View msg) -> View msg
h4_       = mkElement "h4"

public export
h5_       : List (Attr msg) -> List (View msg) -> View msg
h5_       = mkElement "h5"

public export
h6_       : List (Attr msg) -> List (View msg) -> View msg
h6_       = mkElement "h6"

public export
a_        : List (Attr msg) -> List (View msg) -> View msg
a_        = mkElement "a"

public export
button_   : List (Attr msg) -> List (View msg) -> View msg
button_   = mkElement "button"

public export
input_    : List (Attr msg) -> List (View msg) -> View msg
input_    = mkElement "input"

public export
form_     : List (Attr msg) -> List (View msg) -> View msg
form_     = mkElement "form"

public export
ul_       : List (Attr msg) -> List (View msg) -> View msg
ul_       = mkElement "ul"

public export
ol_       : List (Attr msg) -> List (View msg) -> View msg
ol_       = mkElement "ol"

public export
li_       : List (Attr msg) -> List (View msg) -> View msg
li_       = mkElement "li"

public export
section_  : List (Attr msg) -> List (View msg) -> View msg
section_  = mkElement "section"

public export
article_  : List (Attr msg) -> List (View msg) -> View msg
article_  = mkElement "article"

public export
aside_    : List (Attr msg) -> List (View msg) -> View msg
aside_    = mkElement "aside"

public export
main_     : List (Attr msg) -> List (View msg) -> View msg
main_     = mkElement "main"

public export
nav_      : List (Attr msg) -> List (View msg) -> View msg
nav_      = mkElement "nav"

public export
header_   : List (Attr msg) -> List (View msg) -> View msg
header_   = mkElement "header"

public export
footer_   : List (Attr msg) -> List (View msg) -> View msg
footer_   = mkElement "footer"

public export
figure_   : List (Attr msg) -> List (View msg) -> View msg
figure_   = mkElement "figure"

public export
figcaption_ : List (Attr msg) -> List (View msg) -> View msg
figcaption_ = mkElement "figcaption"

public export
img_      : List (Attr msg) -> List (View msg) -> View msg
img_      = mkElement "img"

public export
label_    : List (Attr msg) -> List (View msg) -> View msg
label_    = mkElement "label"

public export
strong_   : List (Attr msg) -> List (View msg) -> View msg
strong_   = mkElement "strong"

public export
em_       : List (Attr msg) -> List (View msg) -> View msg
em_       = mkElement "em"

public export
code_     : List (Attr msg) -> List (View msg) -> View msg
code_     = mkElement "code"

public export
pre_      : List (Attr msg) -> List (View msg) -> View msg
pre_      = mkElement "pre"

public export
blockquote_ : List (Attr msg) -> List (View msg) -> View msg
blockquote_ = mkElement "blockquote"

public export
br_       : List (Attr msg) -> View msg
br_ attrs = mkElement "br" attrs []

public export
hr_       : List (Attr msg) -> View msg
hr_ attrs = mkElement "hr" attrs []

--------------------------------------------------------------------------------
-- Plain attribute helpers (no msg). These wrap HAttr.
--------------------------------------------------------------------------------

attrStr : (name : String) -> (value : String) -> Attr msg
attrStr n v = Plain (MkHAttr n (Str v))

public export
class_       : String -> Attr msg
class_       = attrStr "class"

public export
id_          : String -> Attr msg
id_          = attrStr "id"

public export
href_        : String -> Attr msg
href_        = attrStr "href"

public export
src_         : String -> Attr msg
src_         = attrStr "src"

public export
alt_         : String -> Attr msg
alt_         = attrStr "alt"

public export
title_       : String -> Attr msg
title_       = attrStr "title"

public export
value_       : String -> Attr msg
value_       = attrStr "value"

public export
type_        : String -> Attr msg
type_        = attrStr "type"

public export
name_        : String -> Attr msg
name_        = attrStr "name"

public export
placeholder_ : String -> Attr msg
placeholder_ = attrStr "placeholder"

public export
lang_        : String -> Attr msg
lang_        = attrStr "lang"

public export
role_        : String -> Attr msg
role_        = attrStr "role"

public export
style_       : String -> Attr msg
style_       = attrStr "style"

public export
disabled_    : Attr msg
disabled_    = attrStr "disabled" "disabled"

public export
checked_     : Attr msg
checked_     = attrStr "checked" "checked"

--------------------------------------------------------------------------------
-- Dynamic safety gate (replaces a Phase-2 content-model type).
--------------------------------------------------------------------------------

||| Errors surfaced when a built view does not pass `decideHtml`.
public export
data ViewError : Type where
  ||| The view's tree is not valid HTML under the current spike's
  ||| `IsValidHtml` predicate (i.e. an unknown tag is present). Once
  ||| Phase 2 lands, the located rejection class will carry path +
  ||| reason; for the spike we only know "invalid".
  InvalidHtml : ViewError

public export
Show ViewError where
  show InvalidHtml = "view produced an HExpr that fails IsValidHtml"

||| Pre-Phase-2 dynamic-check fallback. Re-decides the tree's HTML
||| validity and refuses to expose the view if the decision fails.
||| Post-Phase-2 this disappears in favour of compile-time content-model
||| typing on each smart constructor.
public export
viewSafe :
     View msg
  -> Either ViewError ((h : HExpr ** IsValidHtml h), List (String, Event -> msg))
viewSafe (MkView t hs) = case decideHtml t of
  Yes p => Right ((t ** p), hs)
  No  _ => Left  InvalidHtml
