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
