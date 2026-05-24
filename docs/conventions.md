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
| Block quote           | `<blockquote>`             | deferred | Lines prefixed `> `. |
| List (unordered)      | `<ul>` + `<li>`            | deferred | Marker `-` / `*` / `+`. |
| List (ordered)        | `<ol>` + `<li>`            | deferred | Decimal / roman / alpha; `start` carried. |
| List (task)           | `<ul>` + `<li>` w/ checkbox| deferred | Renders `<input type="checkbox" disabled>` first child. |
| List (definition)     | `<dl>` + `<dt>` + `<dd>`   | deferred | `term` carried separately. |
| Code block (fenced)   | `<pre><code>`              | deferred | `info` string becomes language class. |
| Raw block             | (passthrough)              | deferred | Format tag `html` injects literal; other tags suppressed. |
| Div (fenced)          | (see conventions §2)       | deferred | Class drives semantic-element promotion. |
| Pipe table            | `<table>`, `<thead>`, ...  | deferred | Column alignment becomes `style="text-align: ..."`. |
| Reference link def    | (no visible output)        | deferred | Captured for link resolution. |
| Footnote def          | `<aside class="footnote">` | deferred | Anchored by label. |

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
| Footnote reference    | `Text "[label]"`           | placeholder | Will become anchor to footnote def. |
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

| Source                       | Becomes                | Slice    |
|------------------------------|------------------------|----------|
| `:::nav` ... `:::`           | `<nav>`                | deferred |
| `:::aside` ... `:::`         | `<aside>`              | deferred |
| `:::figure` + `:::figcaption`| `<figure>`/`<figcaption>` | deferred |
| `:::main` ... `:::`          | `<main>`               | spike    | Currently inferred as the document wrapper, not a class. |
| `:::header` / `:::footer`    | `<header>` / `<footer>`| deferred |
| `{role="..."}` attribute     | `role` ARIA attribute  | deferred | Validity enforced by Phase 4 ARIA-role check. |
| `{lang="..."}` attribute     | `lang` attribute       | deferred | Phase 4: document-language structural rule. |

The full list lives here; PRs add rows alongside the elaborator slice that
implements them.

---

## 3. Inference rules and the override mechanism

| Inference (deferred)                            | Override |
|-------------------------------------------------|----------|
| Consecutive headings of decreasing level → nested `<section>` | `:::section` fenced div explicitly demarcates. |
| Top-level heading + body → `<main>` wrapper     | `:::main` explicit; or `{role=main}`. |
| Anchor in nav-region → `<nav>` landmark         | `:::nav`. |

The **inference with override** policy (plan.dj §central design commitment):
elaboration infers semantic structure where it is safe and deterministic;
anything inferred can be explicitly overridden by a convention annotation;
ambiguous semantics that cannot be safely inferred require an explicit
convention annotation.

---

## 4. Structural-AA enforcement boundary

> Per plan.dj §central design commitment: **only structurally decidable**
> failures can be elaboration errors. The undecidable ones live in the
> Phase 3 pass.

### Phase 4 Structural set — type-level propositions

All 10 rules below ship as `IsAA_<rule>` propositions in `Cribrum.AA.Typed`
with `Dec` decision procedures. Strict-mode elaboration is intended to
make these hard errors at the elaborate boundary (sharpened `StructuralAA`
codomain) — currently the elaborator still uses the unit placeholder; the
sharpening is the next plan §P4.1 task. Until then, each rule is also
emitted by `Cribrum.AA.Pass`.

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
