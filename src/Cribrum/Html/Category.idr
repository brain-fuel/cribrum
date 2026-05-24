||| HTML content categories per the WHATWG HTML Living Standard
||| (§3.2.5.2). These are the macro-classifications elements belong to,
||| and they drive per-element permitted-content predicates (`Phrasing`-
||| only parents reject `Flow`-only children, etc.).
|||
||| Per `plan.dj` §Phase 2, ingestion ships these from the spec
||| upstreams; this module enumerates the closed set the spec defines.
||| Adding a new category here is a spec change, not a Cribrum change.
module Cribrum.Html.Category

import Data.List

%default total

||| The HTML content categories. An element belongs to zero or more.
public export
data Category
  = ||| Sets the presentation of the rest of the document (`<head>` children).
    Metadata
  | ||| Most elements that appear in the body.
    Flow
  | ||| Elements that define sections in the outline (`section`, `article`, ...).
    Sectioning
  | ||| Heading elements (`h1`-`h6`, `hgroup`).
    Heading
  | ||| Inline-level content (text + most inline elements).
    Phrasing
  | ||| Inserts another resource (`img`, `iframe`, `video`, ...).
    Embedded
  | ||| Reachable by user interaction (`a`, `button`, `input`, ...).
    Interactive
  | ||| Rendered (non-empty) content; relevant for "no empty heading" checks.
    Palpable
  | ||| Helps scripts run (`script`, `template`, `slot`).
    ScriptSupporting
  | ||| Form-associated (subset of Interactive overlapping with `<form>`).
    FormAssociated

public export
Eq Category where
  Metadata         == Metadata         = True
  Flow             == Flow             = True
  Sectioning       == Sectioning       = True
  Heading          == Heading          = True
  Phrasing         == Phrasing         = True
  Embedded         == Embedded         = True
  Interactive      == Interactive      = True
  Palpable         == Palpable         = True
  ScriptSupporting == ScriptSupporting = True
  FormAssociated   == FormAssociated   = True
  _                == _                = False

public export
Show Category where
  show Metadata         = "Metadata"
  show Flow             = "Flow"
  show Sectioning       = "Sectioning"
  show Heading          = "Heading"
  show Phrasing         = "Phrasing"
  show Embedded         = "Embedded"
  show Interactive      = "Interactive"
  show Palpable         = "Palpable"
  show ScriptSupporting = "ScriptSupporting"
  show FormAssociated   = "FormAssociated"

||| `True` iff the two category lists share at least one entry.
||| Used to decide whether a child's categories satisfy a parent's
||| permitted-categories list.
public export
anyOverlap : List Category -> List Category -> Bool
anyOverlap []        _   = False
anyOverlap (c :: cs) ys  = elem c ys || anyOverlap cs ys
