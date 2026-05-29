||| Tests for `Cribrum.Preprocess` — directive recognition, path
||| normalisation, and the resolver-injected recursive splicer. The
||| splice tests run against an in-memory "filesystem" (an assoc list
||| keyed by canonical path), so no IO / fixtures are needed and the
||| real path + cycle logic is exercised exactly as the tool drives it.
module Test.Cribrum.Preprocess

import Data.List
import Data.String
import Hedgehog
import Cribrum.Preprocess

%default total

oneShot : PropertyT () -> Property
oneShot = withTests 1 . property

--------------------------------------------------------------------------------
-- parseIncludeDirective.
--------------------------------------------------------------------------------

export
ext_directive_basic : Property
ext_directive_basic = oneShot $
  parseIncludeDirective "{% include: parts/intro.dj %}"
    === Just "parts/intro.dj"

export
ext_directive_indented : Property
ext_directive_indented = oneShot $
  parseIncludeDirective "   {% include: a.dj %}  " === Just "a.dj"

export
ext_directive_no_spaces : Property
ext_directive_no_spaces = oneShot $
  parseIncludeDirective "{%include:a.dj%}" === Just "a.dj"

||| Trailing text after the closer means it is not a whole-line directive.
export
ext_directive_trailing_text_rejected : Property
ext_directive_trailing_text_rejected = oneShot $
  parseIncludeDirective "{% include: a.dj %} and more" === Nothing

export
ext_directive_missing_closer_rejected : Property
ext_directive_missing_closer_rejected = oneShot $
  parseIncludeDirective "{% include: a.dj" === Nothing

||| Empty path degrades to an ordinary (dropped) Djot comment.
export
ext_directive_empty_path_is_nothing : Property
ext_directive_empty_path_is_nothing = oneShot $
  parseIncludeDirective "{% include: %}" === Nothing

||| A non-include block comment is not a directive.
export
ext_directive_other_comment_is_nothing : Property
ext_directive_other_comment_is_nothing = oneShot $
  parseIncludeDirective "{% just a comment %}" === Nothing

export
ext_directive_plain_text_is_nothing : Property
ext_directive_plain_text_is_nothing = oneShot $
  parseIncludeDirective "include: a.dj" === Nothing

--------------------------------------------------------------------------------
-- normalizePath + dirname.
--------------------------------------------------------------------------------

normCases : List (String, String)
normCases =
  [ ("a/b",        "a/b")
  , ("a/./b",      "a/b")
  , ("a/b/../c",   "a/c")
  , ("./a/b",      "a/b")
  , ("a//b",       "a/b")
  , ("../a",       "../a")
  ]

export
pddt_normalize_path : Property
pddt_normalize_path = oneShot $
  for_ normCases $ \(input, want) => normalizePath input === want

export
ext_normalize_idempotent : Property
ext_normalize_idempotent = oneShot $
  let once = normalizePath "x/./y/../z"
   in normalizePath once === once

export
ext_dirname : Property
ext_dirname = oneShot $ dirname "parts/sub/intro.dj" === "parts/sub"

export
ext_dirname_no_slash : Property
ext_dirname_no_slash = oneShot $ dirname "intro.dj" === ""

--------------------------------------------------------------------------------
-- spliceWith over an in-memory filesystem.
--------------------------------------------------------------------------------

||| In-memory resolver: canonical-key -> contents. Mirrors the tool's
||| pure resolver (normalise `curDir/ref`, look it up, derive baseDir).
fsResolve : List (String, String) -> ResolveFn
fsResolve fs curDir ref =
  let key = resolvePath curDir ref
   in case lookup key fs of
        Just c  => Just (MkResolved key c (dirname key))
        Nothing => Nothing

splice : List (String, String) -> String -> Either IncludeError String
splice fs src = spliceWith (fsResolve fs) 64 [] "" src

export
ext_splice_no_directive_passthrough : Property
ext_splice_no_directive_passthrough = oneShot $
  splice [] "line one\nline two" === Right "line one\nline two"

||| A single include is replaced in place; surrounding lines survive.
export
ext_splice_single_include : Property
ext_splice_single_include = oneShot $
  splice [("part.dj", "SNIPPET")]
         "before\n{% include: part.dj %}\nafter"
    === Right "before\nSNIPPET\nafter"

||| Multiple includes in one file each splice.
export
ext_splice_multiple_includes : Property
ext_splice_multiple_includes = oneShot $
  splice [("a.dj", "AA"), ("b.dj", "BB")]
         "{% include: a.dj %}\n{% include: b.dj %}"
    === Right "AA\nBB"

||| A nested include resolves relative to the *including* snippet's dir.
||| Top file (curDir "") includes `parts/outer.dj`; that snippet (baseDir
||| "parts") includes `inner.dj`, i.e. canonical `parts/inner.dj`.
export
ext_splice_nested_relative_to_includer : Property
ext_splice_nested_relative_to_includer = oneShot $
  splice [ ("parts/outer.dj", "O\n{% include: inner.dj %}")
         , ("parts/inner.dj", "I")
         ]
         "{% include: parts/outer.dj %}"
    === Right "O\nI"

export
ext_splice_cycle_detected : Property
ext_splice_cycle_detected = oneShot $
  splice [ ("a.dj", "{% include: b.dj %}")
         , ("b.dj", "{% include: a.dj %}")
         ]
         "{% include: a.dj %}"
    === Left (IncludeCycle "a.dj" ["b.dj", "a.dj"])

export
ext_splice_missing_include : Property
ext_splice_missing_include = oneShot $
  splice [] "{% include: gone.dj %}"
    === Left (MissingInclude "gone.dj" [])

||| Fuel 0 with a directive present is a depth-exceeded error.
export
ext_splice_depth_exceeded : Property
ext_splice_depth_exceeded = oneShot $
  spliceWith (fsResolve [("a.dj", "X")]) 0 [] "" "{% include: a.dj %}"
    === Left (DepthExceeded "a.dj" [])

export
group : Group
group = MkGroup "Cribrum.Preprocess"
  [ ("ext_directive_basic",                     ext_directive_basic)
  , ("ext_directive_indented",                  ext_directive_indented)
  , ("ext_directive_no_spaces",                 ext_directive_no_spaces)
  , ("ext_directive_trailing_text_rejected",    ext_directive_trailing_text_rejected)
  , ("ext_directive_missing_closer_rejected",   ext_directive_missing_closer_rejected)
  , ("ext_directive_empty_path_is_nothing",     ext_directive_empty_path_is_nothing)
  , ("ext_directive_other_comment_is_nothing",  ext_directive_other_comment_is_nothing)
  , ("ext_directive_plain_text_is_nothing",     ext_directive_plain_text_is_nothing)
  , ("pddt_normalize_path",                     pddt_normalize_path)
  , ("ext_normalize_idempotent",                ext_normalize_idempotent)
  , ("ext_dirname",                             ext_dirname)
  , ("ext_dirname_no_slash",                    ext_dirname_no_slash)
  , ("ext_splice_no_directive_passthrough",     ext_splice_no_directive_passthrough)
  , ("ext_splice_single_include",               ext_splice_single_include)
  , ("ext_splice_multiple_includes",            ext_splice_multiple_includes)
  , ("ext_splice_nested_relative_to_includer",  ext_splice_nested_relative_to_includer)
  , ("ext_splice_cycle_detected",               ext_splice_cycle_detected)
  , ("ext_splice_missing_include",              ext_splice_missing_include)
  , ("ext_splice_depth_exceeded",               ext_splice_depth_exceeded)
  ]
