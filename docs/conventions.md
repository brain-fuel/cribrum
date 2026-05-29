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
| List (unordered)      | `<ul>` + `<li>`            | deferred | Marker `-` / `*` / `+`. |
| List (ordered)        | `<ol>` + `<li>`            | deferred | Decimal / roman / alpha; `start` carried. |
| List (task)           | `<ul>` + `<li>` w/ checkbox| deferred | Renders `<input type="checkbox" disabled>` first child. |
| List (definition)     | `<dl>` + `<dt>` + `<dd>`   | deferred | `term` carried separately. |
| Code block (fenced)   | `<pre><code>`              | spike    | `info` string parsed but not yet promoted to language class (deferred). |
| Raw block             | (passthrough)              | deferred | Format tag `html` injects literal; other tags suppressed. |
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
| Emphasis              | `<em>`                     | spike    | |
| Strong                | `<strong>`                 | spike    | |
| Highlighted           | `<mark>`                   | spike    | |
| Superscript           | `<sup>`                    | spike    | |
| Subscript             | `<sub>`                    | spike    | |
| Insert                | `<ins>`                    | spike    | |
| Delete                | `<del>`                    | spike    | |
| Verbatim              | `<code>`                   | spike    | Attribute block honoured. |
| Link                  | `<a href="...">`           | spike    | Currently passthrough; ref resolution deferred. |
| Image                 | `<img>` (void)             | spike    | Alt source attribute deferred — see §4. |
| Math (inline/display) | `<code>`                   | placeholder | MathJax-style `\\(...\\)` mapping later. |
| Footnote reference    | `<a href="#fn-label"><sup>label</sup></a>` | spike | Intra-document anchor to the `fn-<label>` aside; no upstream-style renumbering. Dangling if no matching def. |
| Symbol (`:name:`)     | `Text ":name:"`            | placeholder | Emoji/symbol table later. |
| Raw inline            | `Text` (passthrough)       | spike    | Format gating deferred. |
| Span                  | `<span>`                   | spike    | Class-driven semantic promotion — see §2. |
| Smart punctuation     | Unicode `Text`             | spike    | “ ” ‘ ’ – — … |

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
promoted to a semantic *phrasing* element by an authoring class.

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
| Document body → single `<main>` wrapper         | done (`elaborateDoc`) | A body that is itself an explicit `<main>` (top-level `:::main`) wins — the wrapper steps aside instead of nesting `<main>` in `<main>`. `{role=main}` + mixed main+sibling layouts deferred. |
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

The strict/draft distinction is enforced at the elaborator boundary; both
modes share the catalog above.
