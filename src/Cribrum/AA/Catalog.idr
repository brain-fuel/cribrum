||| WCAG AA rule catalog — Phase 3 + Phase 4 *shared* source of truth.
|||
||| Per plan.dj §Phase 4: "One shared rule catalog across Phase 1b (elaboration
||| enforcement), Phase 3 (pass), and Phase 4 (types). A rule 'graduates' from
||| finding to proposition; it is never defined twice."
|||
||| This module pins the data describing each rule. The Phase 3 pass
||| (`Cribrum.AA.Pass`) consumes it to emit findings; Phase 4 will key
||| `IsAA_<rule>` propositions to the same rule ids.
module Cribrum.AA.Catalog

%default total

||| Confidence with which a check can claim a verdict. Per plan.dj the
||| heuristic/runtime classes NEVER claim proof status — they are pass-only.
public export
data Confidence
  = ||| Statically decidable on the tree alone; eligible for Phase-4 promotion.
    Structural
  | ||| Needs human judgment (e.g. "alt text is meaningful").
    Heuristic
  | ||| Needs computed style / DOM state (e.g. contrast ratio).
    Runtime

public export
Eq Confidence where
  Structural == Structural = True
  Heuristic  == Heuristic  = True
  Runtime    == Runtime    = True
  _          == _          = False

public export
Show Confidence where
  show Structural = "Structural"
  show Heuristic  = "Heuristic"
  show Runtime    = "Runtime"

public export
data Severity = Error | Warning | Info

public export
Eq Severity where
  Error   == Error   = True
  Warning == Warning = True
  Info    == Info    = True
  _       == _       = False

public export
Show Severity where
  show Error   = "Error"
  show Warning = "Warning"
  show Info    = "Info"

||| Rule metadata. `id` is the stable cross-phase key.
public export
record Rule where
  constructor MkRule
  id          : String
  wcag        : String   -- e.g. "1.1.1"
  level       : String   -- "A" | "AA"
  title       : String
  confidence  : Confidence
  severity    : Severity

public export
Eq Rule where
  (MkRule a b c d e f) == (MkRule x y z w v u) =
    a == x && b == y && c == z && d == w && e == v && f == u

public export
Show Rule where
  show (MkRule i w l t c s) =
    "Rule " ++ i ++ " (WCAG " ++ w ++ " " ++ l ++ ", " ++ show c
      ++ ", " ++ show s ++ "): " ++ t

--------------------------------------------------------------------------------
-- Seed catalog — spike subset.
--------------------------------------------------------------------------------

||| Rules currently checked. The full WCAG AA catalog will be ingested from
||| the W3C's machine-readable ACT rule definitions (per plan §Phase 3).
public export
ruleImgAlt : Rule
ruleImgAlt = MkRule
  { id         = "img-alt"
  , wcag       = "1.1.1"
  , level      = "A"
  , title      = "Images must have an `alt` attribute"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleAnchorHref : Rule
ruleAnchorHref = MkRule
  { id         = "anchor-href"
  , wcag       = "2.4.4"
  , level      = "A"
  , title      = "Anchor (`<a>`) must have an `href` attribute"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleAltMeaningful : Rule
ruleAltMeaningful = MkRule
  { id         = "alt-meaningful"
  , wcag       = "1.1.1"
  , level      = "A"
  , title      = "`alt` text must be meaningful (not just filename or empty)"
  , confidence = Heuristic
  , severity   = Warning
  }

public export
ruleHeadingNoSkip : Rule
ruleHeadingNoSkip = MkRule
  { id         = "heading-no-skip"
  , wcag       = "1.3.1"
  , level      = "A"
  , title      = "Heading levels must not skip (e.g. h1 -> h3 disallowed)"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleDocumentLang : Rule
ruleDocumentLang = MkRule
  { id         = "document-lang"
  , wcag       = "3.1.1"
  , level      = "A"
  , title      = "The `<html>` root must carry a non-empty `lang` attribute"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleIframeTitle : Rule
ruleIframeTitle = MkRule
  { id         = "iframe-title"
  , wcag       = "4.1.2"
  , level      = "A"
  , title      = "Each `<iframe>` must have a non-empty `title` attribute"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleLabelFor : Rule
ruleLabelFor = MkRule
  { id         = "label-for-control"
  , wcag       = "1.3.1"
  , level      = "A"
  , title      = "Each `<label>` must have a `for` attribute or contain its control"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleFieldsetLegend : Rule
ruleFieldsetLegend = MkRule
  { id         = "fieldset-legend"
  , wcag       = "1.3.1"
  , level      = "A"
  , title      = "Each `<fieldset>` should contain a `<legend>` as its accessible name"
  , confidence = Structural
  , severity   = Warning
  }

public export
ruleLinkName : Rule
ruleLinkName = MkRule
  { id         = "link-name"
  , wcag       = "2.4.4"
  , level      = "A"
  , title      = "Each `<a>` with `href` must have accessible text (text content or `aria-label`)"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleButtonName : Rule
ruleButtonName = MkRule
  { id         = "button-name"
  , wcag       = "4.1.2"
  , level      = "A"
  , title      = "Each `<button>` must have an accessible name (text content or `aria-label`)"
  , confidence = Structural
  , severity   = Error
  }

public export
ruleDuplicateId : Rule
ruleDuplicateId = MkRule
  { id         = "duplicate-id"
  , wcag       = "4.1.1"
  , level      = "A"
  , title      = "No two elements may share the same `id`"
  , confidence = Structural
  , severity   = Error
  }

||| All rules in the catalog. The Phase-3 ingestion (per plan.dj §P3.1)
||| will replace this with a generated list emitted from the W3C ACT
||| rules repository; the data shape stays.
public export
allRules : List Rule
allRules =
  [ ruleImgAlt
  , ruleAnchorHref
  , ruleAltMeaningful
  , ruleHeadingNoSkip
  , ruleDocumentLang
  , ruleIframeTitle
  , ruleLabelFor
  , ruleFieldsetLegend
  , ruleLinkName
  , ruleButtonName
  , ruleDuplicateId
  ]
