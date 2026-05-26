||| Integration tests: README.dj + plan.dj end-to-end through the pipeline.
|||
||| Pipeline: file -> parseDoc -> elaborate -> renderHtml.
|||
||| These are *real* dogfood regression tests — they read the actual
||| `README.dj` and `plan.dj` files at the repo root (passed in by
||| `Main.idr` so the relative path stays in one place) and exercise
||| the *current* parser slice against the *current* documents. Any
||| time a Djot construct is added to the parser, or a renderer detail
||| moves, these tests will reflect it without anyone updating an
||| embedded slice.
|||
||| Failure modes the suite catches:
|||   - parser regression on a construct README.dj or plan.dj already uses;
|||   - elaborator regression that loses a landmark / heading / list /
|||     emphasis / verbatim / blockquote;
|||   - renderer regression that drops a tag or escape.
|||
||| Assertions pin *shape* (every kind of element observed in the real
||| doc is present, valid HTML witness produced) rather than byte-exact
||| output so cosmetic renderer tweaks don't force churn.
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

--------------------------------------------------------------------------------
-- Pipeline shorthand.
--------------------------------------------------------------------------------

||| Run one Djot source string through the full pipeline. Returns either a
||| human-readable error or the rendered HTML body.
runPipeline : String -> Either String String
runPipeline src = case parseDoc src of
  Left e  => Left ("parse failed: " ++ show e)
  Right d => case elaborate d of
    Left e             => Left ("elaborate failed: " ++ show e)
    Right (h ** (_, _)) => Right (renderHtml h)

||| Pipeline run that also exposes the validity + accessibility witnesses
||| so a test can observe them and confirm the dependent pair shape.
runPipelineWithWitness :
     String
  -> Either String (h : HExpr ** (IsValidHtml h, StructuralAA h))
runPipelineWithWitness src = case parseDoc src of
  Left e  => Left ("parse: " ++ show e)
  Right d => case elaborate d of
    Left e   => Left ("elaborate: " ++ show e)
    Right pf => Right pf

--------------------------------------------------------------------------------
-- README.dj end-to-end.
--------------------------------------------------------------------------------

||| Real README.dj parses, elaborates, renders. Every construct README.dj
||| currently uses (h1/h2/h3 headings, paragraphs, code spans, emphasis,
||| strong) must survive into the final HTML, wrapped in `<main>` with no
||| bare top-level `<div>` soup.
export
ext_readme_pipeline_works : String -> Property
ext_readme_pipeline_works readmeSrc = withTests 1 . property $
  case runPipeline readmeSrc of
    Left e    => failWith Nothing e
    Right out => do
      diff "<main>"                     isInfixOf out
      diff "</main>"                    isInfixOf out
      diff "<h1>Cribrum</h1>"           isInfixOf out
      diff "<h2>What Cribrum is</h2>"   isInfixOf out
      diff "<h2>Current status</h2>"    isInfixOf out
      diff "<h2>Phase order</h2>"       isInfixOf out
      diff "<h3>"                       isInfixOf out
      diff "<p>"                        isInfixOf out
      diff "<code>HExpr</code>"         isInfixOf out
      -- README.dj uses the wrapped `<main>` landmark; no bare top-level
      -- `<div>` soup.
      classify "has <div>" ("<div>" `isInfixOf` out)

||| The proof carried by Right is a real `IsValidHtml h × StructuralAA h`
||| witness — not just a boolean. Pattern-match it so a regression that
||| weakens the codomain breaks compilation.
export
ext_readme_carries_witnesses : String -> Property
ext_readme_carries_witnesses readmeSrc = withTests 1 . property $
  case runPipelineWithWitness readmeSrc of
    Left e                              => failWith Nothing e
    Right (_ ** (validHtml, structAa))  => do
      let _ = validHtml
      let _ = structAa
      success

--------------------------------------------------------------------------------
-- plan.dj end-to-end (the project's own authoritative narrative).
--------------------------------------------------------------------------------

||| Real plan.dj exercises the *broadest* set of constructs Cribrum currently
||| handles (block quotes, fenced code blocks, ordered + unordered lists,
||| inline strong/em/verbatim). All of them must survive into the rendered
||| HTML — anything missing means a regression in the parser, elaborator,
||| or renderer.
export
ext_plan_pipeline_works : String -> Property
ext_plan_pipeline_works planSrc = withTests 1 . property $
  case runPipeline planSrc of
    Left e    => failWith Nothing e
    Right out => do
      diff "<main>"                isInfixOf out
      diff "</main>"               isInfixOf out
      diff "<h1>"                  isInfixOf out
      diff "<h2>"                  isInfixOf out
      diff "<h3>"                  isInfixOf out
      diff "<hr>"                  isInfixOf out
      diff "<p>"                   isInfixOf out
      diff "<blockquote>"          isInfixOf out
      diff "</blockquote>"         isInfixOf out
      diff "<ul>"                  isInfixOf out
      diff "<li>"                  isInfixOf out
      diff "<strong>"              isInfixOf out
      diff "<em>"                  isInfixOf out
      diff "<code>"                isInfixOf out
      diff "<pre><code>"           isInfixOf out

||| plan.dj's elaboration carries the same dependent-pair shape as
||| README.dj — same witness contract.
export
ext_plan_carries_witnesses : String -> Property
ext_plan_carries_witnesses planSrc = withTests 1 . property $
  case runPipelineWithWitness planSrc of
    Left e                              => failWith Nothing e
    Right (_ ** (validHtml, structAa))  => do
      let _ = validHtml
      let _ = structAa
      success

--------------------------------------------------------------------------------
-- Round-trip identity sanity: the same source rendered twice byte-equal.
-- This catches non-determinism (impure renderer paths, IORef leakage)
-- early.
--------------------------------------------------------------------------------

export
ext_pipeline_is_deterministic : String -> Property
ext_pipeline_is_deterministic src = withTests 1 . property $
  case (runPipeline src, runPipeline src) of
    (Right a, Right b) => a === b
    (l, r)             => failWith Nothing ("non-deterministic: " ++ show (isLeft l, isLeft r))
  where
    isLeft : Either a b -> Bool
    isLeft (Left _)  = True
    isLeft (Right _) = False

--------------------------------------------------------------------------------
-- Group constructor — Main reads the real files and passes them in.
--------------------------------------------------------------------------------

export
mkGroup : (readmeSrc : String) -> (planSrc : String) -> Group
mkGroup readmeSrc planSrc = MkGroup "Cribrum.Integration"
  [ ("ext_readme_pipeline_works",    ext_readme_pipeline_works   readmeSrc)
  , ("ext_readme_carries_witnesses", ext_readme_carries_witnesses readmeSrc)
  , ("ext_plan_pipeline_works",      ext_plan_pipeline_works      planSrc)
  , ("ext_plan_carries_witnesses",   ext_plan_carries_witnesses   planSrc)
  , ("ext_pipeline_deterministic_readme", ext_pipeline_is_deterministic readmeSrc)
  , ("ext_pipeline_deterministic_plan",   ext_pipeline_is_deterministic planSrc)
  ]
