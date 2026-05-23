# Cribrum — Project Plan

> A single intermediate representation — the **HExpr** — authored as prose (Djot), elaborated into
> full semantic, accessible HTML, typechecked as HTML, and checked for WCAG AA accessibility. All
> conformance is expressed as nested refinements over **one** node type, never a family of separate
> types.

**Language:** Idris 2 (core IR, elaboration, type-level conformance, decision procedures). JS via the
Idris 2 JS backend for the final render phase. Cribrum ships a **native Djot parser**; see Phase 1.

**Naming:** the IR node is the **HExpr** ("hypertext expression"). The name signals that the home
model is **HTML** — full semantic hypertext — not a neutral or XML-flavoured tree. Each conformance
layer is a **mesh** (a sieve of increasing fineness): one tree passes through successively finer
meshes — *well-formed ⊇ valid-HTML ⊇ accessible*.

**Scope:** documents only. No XML, no namespaces, no general meta-format machinery. HExpr is full
semantic HTML and nothing wider. (XML appeared earlier only as the thing Djot lacks parity with; it
is explicitly **not** a deliverable.)

---

## Governing principle (read this before writing any code)

There is exactly **one** datatype: the **HExpr**. Its tag space is the **HTML element set**; its
attribute model is the **HTML attribute model** (handler-capable, see below); `div`/`span` are two
elements among many, **not** a catch-all. Everything else — "is valid HTML", "is AA-conformant" — is
a **predicate over HExpr**, never a separate datatype.

- HTML well-formedness is an **indexed proposition** `IsValidHtml : HExpr -> Type` with a decision
  procedure `decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`. Conformance is a *proof*.
- WCAG AA conformance uses a **mixed** strategy:
  - **Structural / statically-decidable** rules are promoted into the type layer using the *same
    machinery* as HTML well-formedness (indexed proposition + `Dec`).
  - **Non-decidable** rules (contrast ratios, "is the alt text meaningful", focus-order sensibility)
    live permanently in a **checking pass** returning findings tagged by confidence. These never
    claim to be proofs.

Layers nest as refinements: any structurally-AA-conformant tree is a valid-HTML tree is a well-formed
HExpr. Build outward from the core; each ring adds constraints without changing the thing being
constrained.

**The single-type invariant (do not violate):** "HTML handling" and "accessibility" are distinct
predicate/render layers over the one HExpr. They are never distinct datatypes. The Djot surface AST
(Phase 1a) is the *only* other tree representation that exists, and it exists solely so the parser is
a correct, ecosystem-compatible Djot parser; everything downstream of elaboration is HExpr.

**Why this matters for downstream code:** a function may demand `(h : HExpr ** IsValidHtml h)` and be
*unable* to receive a malformed tree. The decision procedure manufactures the proof or returns a
located rejection; you never hand-write proof terms per document. This is also what makes the IR
pleasant to manipulate from external libraries (a Rune "Elm++" standard library, Idris 2 view
libraries): they only ever see semantic, valid HTML — never Djot's flat `div`/`span` soup.

---

## The central design commitment: no `div` soup

Djot's natural HTML target is semantically flat — fenced divs and spans with classes. **Cribrum does
not inherit this.** The IR is full *semantic* HTML (`nav`, `article`, `section`, `aside`, `figure`/
`figcaption`, properly labelled controls, correct landmark/heading structure). The flatness is
quarantined entirely inside the Phase 1a surface AST; it never reaches HExpr.

The mechanism is **elaboration** (Phase 1b): `elaborate` takes the faithful Djot surface AST and
produces semantic, accessible HExpr. Two policies govern it, both chosen deliberately:

- **Inference with override.** Elaboration *infers* semantic structure where it is safe and
  deterministic (e.g. heading-level sequences → nested `<section>`/landmark structure). Anything
  inferred can be **explicitly overridden** by a convention annotation in the source. Ambiguous
  semantics that cannot be safely inferred (e.g. "this region is a `<nav>`") require an explicit
  convention annotation.
- **Strict elaboration (accessibility-by-construction).** `elaborate` is **total into
  `(h : HExpr ** IsValidHtml h × StructuralAA h)`**. A document that cannot produce valid, accessible
  HTML is a **hard error** — it never becomes an HExpr. Inaccessible documents are *unrepresentable*,
  not merely flagged.

**Precise scope of "hard error" (critical — get this exactly right):** only the *structurally
decidable* failures can be elaboration errors — e.g. image with no alt source, control with no
associated label, skipped heading level, missing document language, ARIA role on a disallowed
element. The *undecidable* accessibility properties (is the alt text meaningful, is contrast
adequate, is focus order sensible) **cannot** be elaboration errors because elaboration cannot decide
them; they remain Phase 3 pass warnings. Do not attempt to make elaboration decide the undecidable,
and do not let structural failures slip through — both defeat the contract.

**Draft vs. strict mode.** Strict is the **default** and is where the by-construction guarantee
holds. A `draft` mode downgrades the structural hard-errors to located findings so work-in-progress
(a half-captioned `README.dj`) can still render. The guarantee is stated **only** for strict mode.
This is a deliberate feature, not an ad-hoc workaround.

**Authoring uses a convention layer, not stock Djot.** We accept that `.dj` files use class/attribute
idioms that only Cribrum's elaboration understands, trading portability to other Djot tools for the
semantic/accessibility power of the convention layer. No new Djot *surface syntax* is introduced —
the convention layer is classes and attributes the stock grammar already accepts, given meaning by
elaboration. The convention catalog corresponds to HTML semantics and WCAG AA guidelines and is the
heart of the project; it is pinned in `docs/conventions.md`.

---

## Phase order (authoritative — do not resequence)

**1 → 2 → 3 → 4 → 5**, where 3 is the AA *pass* and 4 is "the essence of 2 applied to 3" (the
type-level machinery from Phase 2 reused on the decidable subset of the Phase 3 rules). Rendering is
**last** by design — correctness feedback before Phase 5 comes entirely from checkers and property
tests, so the test harness is load-bearing from day one.

1. Native Djot parser (1a) + elaboration to semantic accessible HExpr (1b)
2. HTML well-formedness in types (comprehensive content model)
3. AA as a checking pass (comprehensive WCAG AA catalog)
4. Promote the decidable AA subset into the type layer (Phase-2 machinery applied to Phase-3 rules)
5. Render to DOM

A coherent, useful system exists after **Phase 2** (proof-carrying valid HTML + README pipeline) and
again after **Phase 3** (plus full accessibility findings). Phases 4 and 5 are additive.

---

## Cross-cutting commitments (apply in every phase)

- **Decision-procedure-returning-`Dec`**, never raw hand-built proofs. Authoring stays normal; the
  checker produces the proof or a precise, *located* reason it cannot.
- **Property-based + mutation testing on the checkers and on elaboration.** Generate arbitrary
  inputs; assert `decideHtml` agrees with an independent oracle; assert `elaborate` output always
  satisfies `IsValidHtml × StructuralAA` (in strict mode) or fails with a located error; mutate the
  checkers/elaborator and confirm tests catch it.
- **Spec-encoding is data-driven, not hand-enumerated.** The HTML content model and the WCAG AA rule
  catalog are large external specifications — ingest them from machine-readable upstreams. The
  testable surface becomes "did my ingestion round-trip", not "did I copy 200 rules correctly".
- **Totality.** Elaboration, render, and all checkers are total. Confine impurity (DOM, FFI) to a
  tiny boundary; never let it leak upward.

---

## Phase 1 — Native Djot parser + elaboration to semantic accessible HExpr

Phase 1 has two clearly separated sub-deliverables. **Do not collapse them**: 1a is faithful to Djot
(and therefore flat); 1b is where flatness becomes semantic accessible HTML.

### Phase 1a — Native Djot parser → Djot surface AST

A native Djot parser producing a **faithful, lossless** parse of `.dj`. This AST has divs/spans/
classes because that is what Djot is. It exists so the parser is a correct, ecosystem-compatible Djot
parser. It is **not** what downstream libraries manipulate.

Source of truth for the grammar: the Djot syntax reference (`doc/syntax.html` in `jgm/djot`). The
surface AST must represent **all** constructs below.

*Block constructs:* Paragraph, Heading, Block quote, List item, List (ordered/unordered/definition/
task, with list-style and start metadata), Code block (with info string), Thematic break, Raw block
(with format tag), Div (fenced, with class/attributes), Pipe table (column alignments, header rows,
caption), Reference link definition, Footnote definition, Block attributes, plus heading-anchor /
auto-identifier handling ("Links to headings").

*Inline constructs:* Ordinary text, Link (inline + reference + autolink), Image (inline + reference),
Autolink, Verbatim (with attributes), Emphasis, Strong, Highlighted, Superscript, Subscript, Insert,
Delete, Smart punctuation (quotes, dashes, ellipsis), Math (inline + display), Footnote reference,
Hard/soft line break, Comment, Symbols (`:name:`), Raw inline (with format tag), Span, Inline
attributes.

*Cross-cutting:* inline **precedence** rules, the **generic attribute** mechanism applied uniformly
to any node, and documented **nesting limits**.

> Sequencing note: a from-scratch parser passing the full reference suite is the single largest piece
> of work in Phase 1 — inline precedence and the attribute mechanism are where the subtlety lives. If
> the native parser threatens to block downstream phases, the instance MAY temporarily FFI a stock
> reference parser (JS/Haskell/Lua) to produce the surface AST and unblock Phases 2–4, landing the
> native parser in parallel. Treat FFI as scaffolding; the end state is the native parser.

**1a acceptance:** parser accepts the Djot reference test-suite input corpus and renders HTML
matching the reference implementation's expected output (divergence is a bug, per the upstream
Jotdown convention). A document exercising **every** construct round-trips through the surface AST.

### Phase 1b — Elaboration → semantic accessible HExpr

`elaborate : DjotSurface -> Either ElabError (h : HExpr ** IsValidHtml h × StructuralAA h)` in strict
mode (the default). This is the seam where:

- Djot's flat `div`/`span` + class conventions are **promoted to semantic HTML elements** (`aside`,
  `nav`, `figure`/`figcaption`, etc.) per the convention catalog.
- Heading-level sequences are **inferred** into nested `<section>`/landmark structure (inference with
  override: an explicit convention annotation overrides the inferred structure).
- Structural accessibility is **enforced by construction** — the structurally-decidable failures are
  hard errors; the output, when it exists, satisfies `IsValidHtml × StructuralAA`.
- `draft` mode downgrades the structural hard-errors to located findings for work-in-progress.

**Convention catalog (`docs/conventions.md`) — pin all of this:**
- The canonical Djot-construct → HExpr-element tag mapping.
- The class/attribute → semantic-element conventions (which class yields `<nav>` vs `<aside>` vs
  `<figure>`, how landmark roles are declared, how labels/alt are supplied, etc.), corresponding to
  HTML semantics and WCAG AA guidelines.
- The inference rules (what is derived from structure) and the override mechanism (how an annotation
  beats inference).
- Which structural-AA properties are enforced at elaboration (the hard-error set) vs. deferred to the
  Phase 3 pass (the undecidable set).
- Round-trip / normalization expectations.

**1b acceptance:**
- `README.dj` (using the convention layer) elaborates to semantic, accessible HExpr and renders to
  HTML with proper landmarks/sectioning — **no bare `div`/`span` soup** in the output.
- In strict mode, `elaborate` never yields an HExpr violating `IsValidHtml × StructuralAA`; deliberate
  bad inputs (image with no alt source, control with no label, skipped heading level) are hard errors
  with located messages.
- In draft mode the same inputs render with located findings instead of failing.
- Property tests: generated surface ASTs either elaborate to conformant HExpr or fail with a located
  error — never produce a non-conformant HExpr in strict mode.

---

## Phase 2 — HTML well-formedness in types (comprehensive)

**Goal:** `IsValidHtml : HExpr -> Type` as an indexed proposition with a total decision procedure
`decideHtml : (h : HExpr) -> Dec (IsValidHtml h)`, over the **full** HTML content model.

**Tasks**
- Encode the content model as **structured data**, ingested from the HTML spec's machine-readable
  element definitions: element catalog; content categories (flow, phrasing, embedded, interactive,
  …); per-element permitted content; per-element permitted attributes.
- Build `IsValidHtml` from sub-relations: tag is a known HTML element; each attribute is permitted on
  its element; children satisfy the parent's content model (the content model **is** a typing
  relation).
- `decideHtml` returns a proof or a **located** rejection (path to offending node + reason).
- This is the **codomain of elaboration**: state and maintain the property that strict `elaborate`
  lands in `(h ** IsValidHtml h)` by construction.

**Acceptance:** full content model decides correctly against a known-valid/known-invalid corpus;
rejection classes exercised and located (block-in-phrasing, disallowed attribute, illegal child for a
structured parent such as non-`li` in `ul` or malformed table structure); property-test oracle agrees
with `decideHtml`; mutation tests catch regressions.

---

## Phase 3 — AA as a checking pass (comprehensive WCAG AA)

**Goal:** `checkAA : (h : HExpr) -> IsValidHtml h -> AAReport`, total, over an already-valid-HTML
tree, interpreting the **full** WCAG AA success-criterion catalog.

**Tasks**
- Encode WCAG AA success criteria as a structured **rule catalog**, ingested from the W3C's
  machine-readable WCAG / ACT rule definitions where available.
- `AAReport` is a list of findings; each carries rule id, located node, severity, and a **confidence
  tag** — `Structural` (statically certain) vs. `Heuristic`/`Runtime` (needs computed style or human
  judgment).
- **Honest partitioning is mandatory.** Contrast ratios, "alt text is meaningful", focus-order
  sensibility are `Heuristic`/`Runtime` findings — reported, never asserted as conformance.
- Note the relationship to Phase 1b: the `Structural` rules here are the *same* properties strict
  elaboration already enforces. Phase 3 is the comprehensive catalog (including the undecidable rules
  elaboration cannot enforce); the `Structural` subset is shared, not duplicated.

**Acceptance:** the AA catalog runs against a corpus with correct pass/fail and correctly-labelled
confidence; the `Structural` subset is explicitly enumerated (input to Phase 4).

---

## Phase 4 — Promote the decidable AA subset into the type layer

**Goal:** "the essence of Phase 2 applied to Phase 3." Reuse the indexed-proposition + `Dec`
machinery from Phase 2 on exactly the `Structural` AA rules from Phase 3 — the same rules strict
elaboration enforces, now expressible as standalone proofs.

**Tasks**
- For each statically-decidable AA rule define `IsAA_<rule> : HExpr -> Type` (or over the HTML proof)
  plus its decision procedure, reusing the Phase-2 framework. Structural subset includes: images have
  `alt`; form inputs have associated labels; heading levels do not skip; document has a language; ARIA
  roles from the valid set on permitted elements; interactive elements keyboard-reachable.
- **One shared rule catalog** across Phase 1b (elaboration enforcement), Phase 3 (pass), and Phase 4
  (types). A rule "graduates" from finding to proposition; it is never defined twice. Single source of
  truth.
- The non-decidable rules **stay** in the Phase 3 pass permanently.

**Acceptance:** structural AA conformance is expressible as a proof in the same framework as HTML
validity; the shared catalog has one source of truth; no rule defined twice; `StructuralAA` (used by
elaboration's codomain) is exactly this set of propositions.

---

## Phase 5 — Render to DOM (reward phase)

**Goal:** put a real page on screen from a (possibly doubly-proof-carrying) HExpr.

**Tasks**
- Total `render : HExpr -> DomEffect` through the Idris 2 → JS backend.
- Tiny, total FFI surface: `createElement`, `setAttribute`, `addEventListener` (posts a message),
  `replaceChild`. Nothing more.
- Diff strategy: start with full re-render on update **or** keyed children + shallow structural diff.
  **Do not** build a full virtual DOM until something forces it.
- Port one small fragment of a real front-end (not a toy) end-to-end.

**Acceptance:** a `.dj`-or-hand-built document, proven valid HTML (ideally carrying structural-AA
proofs), renders live in a browser.

---

## Repository layout (suggested)

```
cribrum/
  src/
    Cribrum/Node.idr            -- the HExpr type (HTML element tag space + handler-capable attrs)
    Cribrum/Traversal.idr       -- walk / rewrite / query / zipper
    Cribrum/Djot/Surface.idr    -- the faithful Djot surface AST (Phase 1a)
    Cribrum/Djot/Parser.idr     -- native Djot parser -> surface AST
    Cribrum/Djot/Grammar.idr    -- inline precedence, construct inventory, nesting limits
    Cribrum/Elaborate.idr       -- elaborate : DjotSurface -> Either ElabError (h ** valid x AA)
    Cribrum/Conventions.idr     -- class/attribute -> semantic element + inference/override rules
    Cribrum/Render/Djot.idr     -- HExpr/surface -> Djot (round-trip)
    Cribrum/Render/Html.idr     -- HExpr -> HTML (reference-suite parity)
    Cribrum/Html/Model.idr      -- ingested content-model data
    Cribrum/Html/Valid.idr      -- IsValidHtml + decideHtml
    Cribrum/AA/Catalog.idr      -- shared WCAG AA rule catalog (single source of truth)
    Cribrum/AA/Pass.idr         -- checkAA + AAReport
    Cribrum/AA/Typed.idr        -- IsAA_<rule> + decision procedures; StructuralAA (Phase 4)
    Cribrum/Render/Dom.idr      -- render + tiny DOM FFI (Phase 5)
  ingest/                       -- scripts pulling HTML content model + WCAG AA from upstreams
  docs/
    conventions.md              -- THE convention catalog: Djot/class -> semantic HTML + a11y, pinned
  test/
    corpus/                     -- known-valid / known-invalid HTML, AA pass/fail fixtures
    djot-ref/                   -- Djot reference test suite (input + expected HTML)
    Props/                      -- property-based + mutation test suites
  README.dj                     -- dogfood: the project's own README, in the Djot convention layer
```

## Definition of done (v1)

- Phases 1–4 complete; Phase 5 renders at least one real fragment.
- Native Djot parser covers the **full** construct inventory and passes the reference suite (surface
  AST, Phase 1a).
- Elaboration produces semantic, accessible HExpr with **no `div`/`span` soup**; strict mode is
  accessibility-by-construction (hard error on structurally-decidable failures); draft mode degrades
  to findings.
- `README.dj` is the project's own README, in the convention layer, proving the pipeline on itself.
- Content model and WCAG AA catalog are **ingested**, not hand-transcribed, with round-trip tests.
- Every checker and elaboration has property + mutation tests; the harness is the primary oracle.

## Explicit non-goals (v1)

- **No XML, no namespaces, no meta-format generality.** HExpr is full semantic HTML and nothing wider.
- **No second datatype.** The HExpr is the only IR; the Djot surface AST exists solely for faithful
  parsing/round-tripping. "HTML handling" and "accessibility" are predicate/render layers over HExpr,
  never separate types.
- **No new Djot surface syntax.** The convention layer is classes/attributes the stock grammar already
  accepts, given meaning by elaboration. Extensions happen at the elaboration/AST layer only.
- **No portability guarantee to other Djot tools.** We deliberately trade `.dj` portability for the
  convention layer's semantic/accessibility power.
- **No full virtual DOM** in Phase 5 unless a concrete need forces it.
- **No claim that heuristic/runtime AA findings are proofs**, and no attempt to make elaboration
  decide undecidable accessibility properties.
