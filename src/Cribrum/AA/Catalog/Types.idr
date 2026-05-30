||| Types layer for the AA rule catalog — extracted so the auto-generated
||| `Cribrum.AA.Catalog.Generated` module can depend on the data shape
||| without creating a cycle through `Cribrum.AA.Catalog`. Mirrors the
||| `Cribrum.Html.Model.Types` split.
|||
||| The public face of the catalog is `Cribrum.AA.Catalog`, which re-
||| exports both these types and the generated rule data.
module Cribrum.AA.Catalog.Types

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

||| Provenance of a catalog rule. Hand-curated Cribrum rules are
||| `Cribrum`; rows ingested from the upstream ACT-rules corpus
||| (`ingest/act-rules.ts`, plan §P3.1) are `Act`. Lets audits and the
||| Phase-4 promotion logic tell the data-entry layer apart from the
||| hand-typed core.
public export
data RuleSource = Cribrum | Act

public export
Eq RuleSource where
  Cribrum == Cribrum = True
  Act     == Act     = True
  _       == _       = False

public export
Show RuleSource where
  show Cribrum = "Cribrum"
  show Act     = "Act"

||| Rule metadata. `id` is the stable cross-phase key.
|||
||| Fields `source`..`expectation` carry ACT-rules provenance and prose. For
||| hand-curated Cribrum rows they default to `Cribrum` / empty; ACT rows
||| populate them from the upstream front-matter + Applicability/Expectation
||| sections. The prose is stored as text for now (plan §P3.1: data before
||| interpretation) so the Idris side can later promote a tree-decidable
||| `expectation` from Heuristic to Structural.
public export
record Rule where
  constructor MkRule
  id            : String
  wcag          : String   -- primary WCAG SC, e.g. "1.1.1"
  level         : String   -- "A" | "AA"
  title         : String
  confidence    : Confidence
  severity      : Severity
  source        : RuleSource
  actId         : String       -- upstream ACT 6-char id, "" for Cribrum rows
  ruleType      : String       -- ACT rule_type ("atomic"/...), "" otherwise
  wcagAll       : List String  -- every forConformance SC the rule maps to
  applicability : String       -- ACT Applicability prose (flattened), "" otherwise
  expectation   : String       -- ACT Expectation prose (flattened), "" otherwise

public export
Eq Rule where
  (MkRule a b c d e f g h i j k l) == (MkRule a' b' c' d' e' f' g' h' i' j' k' l') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'
      && g == g' && h == h' && i == i' && j == j' && k == k' && l == l'

public export
Show Rule where
  show r =
    "Rule " ++ r.id ++ " (WCAG " ++ r.wcag ++ " " ++ r.level ++ ", "
      ++ show r.confidence ++ ", " ++ show r.severity ++ ", "
      ++ show r.source ++ "): " ++ r.title
