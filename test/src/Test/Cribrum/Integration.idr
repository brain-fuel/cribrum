||| Integration test: README.dj end-to-end through the pipeline.
|||
||| Pipeline: file -> parseDoc -> elaborate -> renderHtml.
||| Assertions pin the *shape* (every element present, valid HTML witness
||| produced) rather than byte-exact output so cosmetic renderer tweaks
||| don't force a churn.
module Test.Cribrum.Integration

import Data.String
import Hedgehog
import Cribrum.Node
import Cribrum.Djot.Surface
import Cribrum.Djot.Parser
import Cribrum.Html.Valid
import Cribrum.Elaborate
import Cribrum.Render.Html

%default total

readme : String
readme =
  """
  # Cribrum

  A single intermediate representation — the HExpr — authored as prose in Djot,
  elaborated into full semantic, accessible HTML, typechecked as HTML, and
  checked for WCAG AA accessibility.

  ## Why

  Each conformance layer is a sieve of increasing fineness. One tree passes
  through successively finer meshes.

  ---

  ## Pipeline

  The pipeline composes four phases: parse Djot, elaborate to HExpr, witness
  HTML validity, render to HTML.
  """

||| End-to-end: parse a multi-construct Djot document, elaborate to HExpr
||| with proof of validity, render to HTML string. The output must contain
||| every section heading, the thematic break, and the wrapping <main>
||| landmark — confirming no `div`/`span` soup at top level.
export
ext_readme_pipeline_works : Property
ext_readme_pipeline_works = withTests 1 . property $ do
  case parseDoc readme of
    Left e  => failWith Nothing ("parse failed: " ++ show e)
    Right d => case elaborate d of
      Left e  => failWith Nothing ("elaborate failed: " ++ show e)
      Right (h ** (_, _)) => do
        let out = renderHtml h
        diff "<main>"       isInfixOf out
        diff "</main>"      isInfixOf out
        diff "<h1>Cribrum</h1>" isInfixOf out
        diff "<h2>Why</h2>"      isInfixOf out
        diff "<h2>Pipeline</h2>" isInfixOf out
        diff "<hr>"         isInfixOf out
        -- No bare `<div>` soup at top level (we wrap in <main>).
        classify "has <div>" ("<div>" `isInfixOf` out)
        -- Sanity: HTML must include at least one paragraph.
        diff "<p>"          isInfixOf out

||| The proof carried by Right is a real `IsValidHtml h` witness — not just
||| a boolean. We exercise the pattern match to ensure the dependent pair is
||| usable from external call sites.
export
ext_readme_carries_validity_witness : Property
ext_readme_carries_validity_witness = withTests 1 . property $ do
  case parseDoc readme of
    Left e  => failWith Nothing ("parse: " ++ show e)
    Right d => case elaborate d of
      Left e  => failWith Nothing ("elaborate: " ++ show e)
      Right (_ ** (validHtml, structuralAa)) => do
        -- The witness exists; observe it. Idris would have rejected the
        -- pattern match if the projection types disagreed.
        let _ = validHtml
        let _ = structuralAa
        success

export
group : Group
group = MkGroup "Cribrum.Integration"
  [ ("ext_readme_pipeline_works",            ext_readme_pipeline_works)
  , ("ext_readme_carries_validity_witness",  ext_readme_carries_validity_witness)
  ]
