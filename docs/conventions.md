# Cribrum Convention Catalog

> The convention layer **is the heart of the project**. Authoring uses Djot's
> stock grammar; the conventions below give meaning to particular class /
> attribute idioms during elaboration (`Cribrum.Elaborate`, Phase 1b).
>
> **No new Djot surface syntax is introduced** (plan.dj §central design
> commitment). Everything here is class-and-attribute idioms the stock Djot
> parser already accepts.

This document is the **pinned** catalog per plan.dj §Phase 1b. It is the
single source of truth for:

1. The canonical Djot-construct → HExpr-element tag mapping.
2. The class/attribute → semantic-element conventions.
3. Inference rules (what is derived from structure) and the override mechanism
   (how an annotation beats inference).
4. Which structural-AA properties are enforced at elaboration (the hard-error
   set, strict mode) vs. deferred to the Phase 3 pass (the undecidable set).
5. Round-trip / normalization expectations.

The catalog grows construct-by-construct as parser + elaborator slices land;
sections marked **(spike)** describe what the current slice ships, sections
marked **(deferred)** describe the contract that future iterations will
honour.

---

## 1. Djot-construct → HExpr-element mapping

### Block constructs

| Djot construct        | HExpr element              | Slice    | Notes |
|-----------------------|----------------------------|----------|-------|
| Paragraph             | `<p>`                      | spike    | Inlines map child-wise. |
| Heading (level n)     | `<h1>`...`<h6>`            | spike    | Levels 1..6; >6 currently parses as paragraph. |
| Thematic break        | `<hr>`                     | spike    | `---` / `***`, 3+ chars, surrounding ws ok. |
| Block quote           | `<blockquote>`             | spike    | Lines prefixed `> `; recursive nesting supported. |
| List (unordered)      | `<ul>` + `<li>`            | done     | Marker `-` / `*` / `+`; nested sub-lists, multi-paragraph items, lazy continuation, tight/loose detection (blank before a sub-list stays tight). A bullet-family change starts a new list. A `{...}` block-attribute prefix attaches to the list (`class`/`id`/pairs emitted on the list element). |
| List (ordered)        | `<ol>` + `<li>`            | done     | Decimal / lower+upper roman / lower+upper alpha; delimiters `1.` `1)` `(1)`. `start` carried + emitted (omitted when 1); non-decimal styles emit `type=` (`a`/`A`/`i`/`I`). Ambiguous leading roman/alpha letter resolved against the second item, defaulting to roman. A marker an ordered value ≠ 1 cannot interrupt an open paragraph. A `{...}` prefix's attrs are emitted before `start`/`type`. |
| List (task)           | `<ul class="task-list">` + `<li class="checked\|unchecked">` | done | Promoted when every item is a `[ ]`/`[x]` checkbox; loose task lists keep the `<p>` wrap. |
| List (definition)     | `<dl>` + `<dt>` + `<dd>`   | done     | `term` carried separately. |
| Code block (fenced)   | `<pre><code>`              | spike    | `info` string parsed but not yet promoted to language class (deferred). |
| Raw block             | `Raw` (passthrough)        | spike    | `=html` fenced block (`` ``` =html ``) injects its body verbatim as an unescaped `Raw` node; any other format is suppressed (no output). |
| Div (fenced)          | (see conventions §2)       | spike    | Convention class drives semantic-element promotion; else `<div>`. |
| Pipe table            | `<table>`, `<thead>`, ...  | deferred | Column alignment becomes `style="text-align: ..."`. |
| Reference link def    | (no visible output)        | deferred | Captured for link resolution. |
| Footnote def          | `<aside class="footnote">` | spike    | `id="fn-<label>"`; label-anchored. Diverges from upstream Djot's numbered `<section role="doc-endnotes">` (see §3). |

### Inline constructs

| Djot construct        | HExpr element              | Slice    | Notes |
|-----------------------|----------------------------|----------|-------|
| Text                  | `Text`                     | spike    | Verbatim payload. |
| Soft line break       | `Text " "`                 | spike    | Single space — standard Djot HTML behaviour. |
| Hard line break       | `<br>`                     | spike    | Void element. |
| Comment               | `Comment`                  | spike    | HTML comment node. |
| Emphasis              | `<em>`                     | spike    | Flanking by inside-whitespace agreement; closer scan skips backslash-escaped markers (`_\__` → `<em>_</em>`). |
| Strong                | `<strong>`                 | spike    | As emphasis (`*`). |
| Highlighted           | `<mark>`                   | spike    | |
| Superscript           | `<sup>`                    | spike    | `^x^`; braced `{^ x ^}` keeps inner whitespace. No flanking restriction. |
| Subscript             | `<sub>`                    | spike    | `~x~`; braced `{~ x ~}` keeps inner whitespace. No flanking restriction. |
| Insert                | `<ins>`                    | spike    | |
| Delete                | `<del>`                    | spike    | |
| Verbatim              | `<code>`                   | spike    | Attribute block honoured. |
| Link                  | `<a href="...">`           | spike    | Currently passthrough; ref resolution deferred. |
| Image                 | `<img>` (void)             | spike    | Alt source attribute deferred — see §4. |
| Math (inline/display) | `<span class="math inline">` / `<span class="math display">` | spike | `` $`…` `` -> `\(…\)`; `` $$`…` `` -> `\[…\]`. Verbatim body (raw, not markup-parsed); `<`/`>`/`&` entitised. A `$` not before a backtick stays literal. |
| Footnote reference    | `<a href="#fn-label"><sup>label</sup></a>` | spike | Intra-document anchor to the `fn-<label>` aside; no upstream-style renumbering. Dangling if no matching def. |
| Symbol (`:name:`)     | `Text ":name:"`            | placeholder | Emoji/symbol table later. |
| Raw inline            | `Raw` (passthrough)        | spike    | `` `…`{=html} `` injects its content verbatim as an unescaped `Raw` node; any other format is suppressed. A `{=fmt …}` brace carrying anything beyond the bare `=fmt` token falls back to ordinary verbatim. |
| Span                  | `<span>`                   | spike    | Class-driven semantic promotion — see §2. |
| Smart punctuation     | Unicode `Text` → named entity | spike    | Quote orientation by before/after flanking; `'`+digit/elision (`'70s`, `'tis`) is apostrophe. Dash runs split per Djot (÷3 em, ÷2 en, else em+en mix). Rendered as `&ldquo;`/`&rsquo;`/`&ndash;`/`&mdash;`/`&hellip;` (matches djot.js). |
| Backslash escape      | literal char in `Text`     | spike    | `\` + ASCII punctuation → literal char, suppresses markup/smart processing; `\` + other → literal `\`. (`\ `=space hard-break/nbsp deferred.) |

---

## 2. Class / attribute conventions (semantic promotion)

> Goal per plan.dj §central design commitment: **no `div`/`span` soup**.
> Elaboration promotes Djot's flat div/span into proper landmark / sectioning
> elements based on class hints.

The convention layer is the *mechanism* by which Cribrum delivers semantic
HTML from a stock-Djot input. Each entry is an EXPLICIT convention: an
authoring class that elaboration consumes (and removes from the output) to
emit a specific semantic element.

| Source                       | Becomes                | Slice    | Notes |
|------------------------------|------------------------|----------|-------|
| `:::nav` ... `:::`           | `<nav>`                | spike    | First convention class wins; consumed from `class`. |
| `:::aside` ... `:::`         | `<aside>`              | spike    | |
| `:::figure` + `:::figcaption`| `<figure>`/`<figcaption>` | spike | Each div promotes independently; nest `:::figcaption` inside `:::figure`. |
| `:::main` ... `:::`          | `<main>`               | spike    | Explicit override of the §3 `<main>`-wrapper inference. |
| `:::header` / `:::footer`    | `<header>` / `<footer>`| spike    | |
| `:::section` ... `:::`       | `<section>`            | spike    | Explicit override of the §3 heading→`<section>` inference. |
| `{role="..."}` attribute     | `role` ARIA attribute  | spike    | Rides through as a pair attr; validity enforced by Phase 4 ARIA-role check. |
| `{lang="..."}` attribute     | `lang` attribute       | spike    | Rides through as a pair attr; Phase 4: document-language structural rule. |

Promotion rule (`Cribrum.Elaborate.promoteDiv`): the **first** class in
source order matching the convention set (`nav`, `aside`, `figure`,
`figcaption`, `header`, `footer`, `section`, `main`) drives the element tag
and is **removed** from the emitted `class`; remaining classes survive in
order. No convention class → plain `<div>`. Other annotations (`id`, `role`,
`lang`, arbitrary `key=value`) pass through untouched via `attrsToHAttrs`.

The same promotion applies on the **inline** axis: a `[text]{.cls}` span is
promoted to a semantic *phrasing* element by an authoring class. An inline
attribute block also attaches to the immediately-preceding **word** when not
bracketed (`word{.cls}` wraps just that word in a span); a block with nothing
to attach to (at line start, or preceded by whitespace) or an empty `{}` is
dropped, leaving the surrounding text intact. Quoted values may carry spaces,
braces, and escaped `\"`; `%…%` comments inside the block are ignored.

| Source                       | Becomes                | Slice    | Notes |
|------------------------------|------------------------|----------|-------|
| `[..]{.abbr}`                | `<abbr>`               | spike    | First convention class wins; consumed from `class`. |
| `[..]{.cite}`                | `<cite>`               | spike    | |
| `[..]{.dfn}`                 | `<dfn>`                | spike    | Defining instance of a term. |
| `[..]{.kbd}`                 | `<kbd>`                | spike    | |
| `[..]{.samp}`                | `<samp>`               | spike    | Sample / program output. |
| `[..]{.var}`                 | `<var>`                | spike    | |
| `[..]{.time}`                | `<time>`               | spike    | `{datetime=}` rides through as a pair attr. |
| `[..]{.q}`                   | `<q>`                  | spike    | Inline quotation. |

Span promotion rule (`Cribrum.Elaborate.promoteSpan`): the inline mirror of
`promoteDiv` — the **first** class matching the span convention set (`abbr`,
`cite`, `dfn`, `kbd`, `samp`, `var`, `time`, `q`) drives the phrasing tag and
is **removed** from the emitted `class`; remaining classes survive in order.
No convention class → plain `<span>`. `id` and arbitrary `key=value` pairs
pass through untouched via `attrsToHAttrs`.

The full list lives here; PRs add rows alongside the elaborator slice that
implements them.

---

## 3. Inference rules and the override mechanism

| Inference                                       | Status | Override |
|-------------------------------------------------|--------|----------|
| Consecutive headings of decreasing level → nested `<section>` | done (`sectionize`) | `:::section` fenced div explicitly demarcates. |
| Document body → single `<main>` wrapper         | done (`elaborateDoc`) | A body that is itself an explicit main landmark wins — the wrapper steps aside instead of producing two mains. Two forms count: a top-level `:::main` (→ `<main>` tag) **and** a top-level element carrying `role="main"` (the ARIA form, e.g. `:::{role=main}`). Both decided by `isMainLandmark`. Mixed main+sibling layouts stay deferred. |
| "This region is a `<nav>`"                       | **not inferred** — explicit-only | `:::nav` (§2). Per plan.dj §central design commitment, nav semantics cannot be safely inferred from structure and *require* an explicit convention annotation. |

The **inference with override** policy (plan.dj §central design commitment):
elaboration infers semantic structure where it is safe and deterministic
(heading runs → `<section>`, document body → `<main>`); anything inferred can
be explicitly overridden by a convention annotation; ambiguous semantics that
cannot be safely inferred — canonically "this region is a `<nav>`" — are *not*
guessed at and require an explicit convention annotation instead.

---

## 3a. Source composition (`{% include: … %}`)

A whole `.dj` document may be assembled from snippet files. Djot has no native
include syntax, so this is a **textual pre-pass** (`tools/preprocess`, pure core
`Cribrum.Preprocess`) run *before* `parseDoc`:

```
{% include: parts/intro.dj %}
```

- **Whole-line directive.** The trimmed line must be exactly
  `{% include: PATH %}`. Paths are resolved **relative to the including file's
  directory** (nested snippets resolve relative to their own location).
- **One document, one `<main>`.** Because the spliced result is a single source
  fed to one `parseDoc → elaborateDoc`, the output has exactly one `<main>` by
  construction — splicing cannot create nested `<main>`.
- **Snippets must not declare their own `<main>`** (no top-level `:::main` in a
  snippet): the host document owns the single landmark. A stray one is caught
  downstream — `elaborate`'s `unique-main` rule hard-errors — so this is a
  documented authoring rule, not enforced by the preprocessor.
- **Graceful degradation.** `{% include: … %}` is a valid Djot block comment, so
  an *un-preprocessed* file simply drops the line rather than emitting garbage.
- **Errors** (`tools/preprocess` exits non-zero): missing include, include
  cycle, and depth-exceeded, each reported with the ancestor chain.

---

## 4. Structural-AA enforcement boundary

> Per plan.dj §central design commitment: **only structurally decidable**
> failures can be elaboration errors. The undecidable ones live in the
> Phase 3 pass.

### Phase 4 Structural set — type-level propositions

All 10 rules below ship as `IsAA_<rule>` propositions in `Cribrum.AA.Typed`
with `Dec` decision procedures. Strict-mode elaboration makes these hard
errors at the elaborate boundary: `Cribrum.Elaborate.StructuralAA` is the
real conjunct of all 10 propositions, decided by `decStructuralAA`; strict
`elaborate` fails with `StructuralAaFailure ruleId path` on the first
failing predicate (per-node rules carry `Just path`, root-only
`document-lang` carries `Just []`, whole-tree rules carry `Nothing`). Each
rule is also emitted by `Cribrum.AA.Pass` for the draft-mode / reporting
path.

| Rule id            | What                                              | Phase 3 (Pass) | Phase 4 (Typed) |
|--------------------|---------------------------------------------------|----------------|-----------------|
| `img-alt`          | Image must have an alt source.                    | ✅              | ✅               |
| `anchor-href`      | Anchor must have an href.                         | ✅              | ✅               |
| `heading-no-skip`  | Heading levels must not skip (h1 → h3 disallowed).| ✅              | ✅               |
| `label-for-control`| Form control must have an associated label.      | ✅              | ✅               |
| `document-lang`    | Document root must declare a language.            | ✅              | ✅               |
| `iframe-title`     | Each `<iframe>` must have a non-empty title.      | ✅              | ✅               |
| `fieldset-legend`  | Each `<fieldset>` must contain a `<legend>`.      | ✅              | ✅               |
| `button-name`      | Each `<button>` must have an accessible name.     | ✅              | ✅               |
| `link-name`        | Each `<a href>` must have an accessible name.     | ✅              | ✅               |
| `duplicate-id`     | No two elements may share the same `id`.          | ✅              | ✅               |
| `unique-main`      | At most one `<main>` landmark per document.       | ✅              | ✅               |

Partitioning audit (plan §P3.3) is enforced by `Test.Cribrum.AA.Partition`:
`Cribrum.AA.Typed.isTypedPromoted` is the witness, and the test suite asserts
it agrees with `confidence == Structural` over every entry in `allRules`.
Adding a Structural rule without a typed promotion (or vice versa) fails the
audit.

### Phase 3 pass only (never claimed as proof)

| Rule id          | What                                              | Confidence |
|------------------|---------------------------------------------------|------------|
| `alt-meaningful` | `alt` text must be meaningful, not a filename.    | Heuristic  |
| `contrast-ratio` | WCAG contrast ratio (4.5:1 body / 3:1 large).     | Runtime    |
| `focus-order`    | Focus order matches visual order.                 | Runtime    |

---

## 5. Round-trip / normalization expectations

Goals (plan.dj §1a acceptance):

- The parser accepts the Djot reference test-suite input corpus and renders
  HTML matching the reference implementation's expected output.
- A document exercising every Djot construct round-trips through the surface
  AST. (Spike: only the constructs the parser supports today round-trip; this
  list grows as slices land.)
- Reference link definitions and footnote definitions normalise to a single
  canonical form (deferred until those constructs land).

---

## Draft mode

`draft` mode (plan.dj §central design commitment) downgrades the structural
hard-errors to located findings so work-in-progress (a half-captioned
`README.dj`) can still render. Strict mode is the **default** and is where
the by-construction guarantee holds. The guarantee is stated **only** for
strict mode.

`Cribrum.Elaborate.elaborateDraft` implements this as a **placeholder
repair**: it rewrites the structurally-decidable author mistakes into valid
stand-ins, each tagged with a visible `data-cribrum-todo` attribute and
reported as a `DraftWarning` (a `todo!()`-style record: path + rule +
diagnosis + the fix to apply). The return type stays proof-carrying —
`(h ** (IsValidHtml h, StructuralAA h, List DraftWarning))` — so the draft
output is *itself* valid HTML, just unmistakably unfinished. This is the
"clear first, then fix" loop: render with placeholders, then resolve each
TODO. Repaired patterns:

| Mistake | Strict (hard error) | Draft (placeholder + warning) |
|---------|---------------------|-------------------------------|
| disallowed attribute `x="v"` | `DisallowedAttr tag x` | renamed to `data-x="v"` (value kept) |
| interactive element inside `<a>` | `InteractiveInInteractive` | demoted to `<span>` (its `href` → `data-href`) |
| `<a>` with no / empty href | `anchor-href` / `link-empty-href` | `href="#"` |

Anything draft cannot repair (unknown tag, illegal nesting, duplicate id,
heading skip, …) still hard-errors — draft fixes author slips, it is not a
"make any tree valid" escape hatch. Strict-mode errors themselves now carry a
localized, semantic message (`explainReject` / `explainRule` + the tree
path), e.g. *"attribute `key` is not a valid HTML attribute on `<span>` … at
child path [0, 0]"*.

The strict/draft distinction is enforced at the elaborator boundary; both
modes share the catalog above.
