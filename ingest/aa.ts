// WCAG AA rule catalog — hand-curated source of truth for the data that
// the W3C ACT-rules repository would otherwise supply. Plan.dj §P3.1
// targets ACT-rule ingestion; this file is the typed staging area that
// the upstream pull will populate row-by-row.
//
// Each row mirrors the `Rule` record in `Cribrum.AA.Catalog`. The
// ingestion script (`ingest/aa-catalog.ts`) cross-validates structural
// invariants and compiles a deterministic
// `src/Cribrum/Html/../AA/Catalog/Generated.idr` from this table.
//
// Migration path: as the upstream ACT-rules JSON pull lands, rows here
// shrink to the override layer (Cribrum-specific severity overrides,
// confidence reclassifications justified by our type system, etc).

export type Confidence = "Structural" | "Heuristic" | "Runtime";
export type Severity   = "Error" | "Warning" | "Info";

// Provenance of a catalog row. Hand-curated Cribrum rules are "cribrum";
// rows pulled from the upstream ACT-rules corpus (ingest/act-rules.ts) are
// "act". Carried into the generated module so the Idris side and audits can
// tell apart the data-entry layer from the hand-typed core.
export type RuleSource = "cribrum" | "act";

export interface AARuleRow {
  // Stable cross-phase key. Matches the `id` field on Idris-side `Rule`.
  id:          string;
  // WCAG SC number, e.g. "1.1.1".
  wcag:        string;
  // WCAG conformance level: "A" or "AA".
  level:       "A" | "AA";
  // Human-readable title (single-line; emitted into the generated module
  // verbatim, so keep free of unescaped quotes).
  title:       string;
  confidence:  Confidence;
  severity:    Severity;

  // --- Provenance + ACT-rules carry-data (optional; defaulted on emit) ----
  // Where the row came from. Defaults to "cribrum" when omitted.
  source?:        RuleSource;
  // Upstream ACT 6-char rule id (act rows only), e.g. "97a4e1".
  actId?:         string;
  // Upstream ACT rule_type ("atomic" | "composite"); act rows only.
  ruleType?:      string;
  // Every forConformance WCAG SC the rule maps to (act rows may map to
  // several; `wcag` above is the chosen primary). Plain Cribrum rows omit.
  wcagAll?:       string[];
  // Applicability prose (which nodes the rule applies to), flattened to a
  // single line. Carried as data so the Idris side can later interpret it.
  applicability?: string;
  // Expectation prose (the pass condition), flattened to a single line.
  expectation?:   string;
}

// Rows sorted by `id` lexicographically — determinism for byte-identical
// regeneration. Adding a rule: insert in sorted position.
export const AA_CATALOG: AARuleRow[] = [
  { id: "alt-meaningful",
    wcag: "1.1.1", level: "A",
    title: "`alt` text must be meaningful (not just filename or empty)",
    confidence: "Heuristic", severity: "Warning" },

  { id: "anchor-href",
    wcag: "2.4.4", level: "A",
    title: "Anchor (`<a>`) must have an `href` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "area-alt",
    wcag: "1.1.1", level: "A",
    title: "Each `<area>` with `href` must have an `alt` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "aria-hidden-body",
    wcag: "4.1.2", level: "A",
    title: "`<body>` must not carry `aria-hidden=\"true\"` (hides the whole page from assistive tech)",
    confidence: "Structural", severity: "Error" },

  { id: "aria-label-redundant",
    wcag: "4.1.2", level: "AA",
    title: "`aria-label` should not duplicate the element's visible text",
    confidence: "Heuristic", severity: "Warning" },

  { id: "aria-role-valid",
    wcag: "4.1.2", level: "A",
    title: "A `role` attribute value must be a defined WAI-ARIA role token",
    confidence: "Structural", severity: "Error" },

  { id: "audio-description",
    wcag: "1.2.5", level: "AA",
    title: "Audio Description (Prerecorded): synchronised audio description for prerecorded video",
    confidence: "Runtime", severity: "Info",
    applicability: "Prerecorded synchronised-media video content.",
    expectation: "An audio description track conveys the important visual information not in the main audio." },

  { id: "autocomplete-valid",
    wcag: "1.3.5", level: "AA",
    title: "An `autocomplete` value must be a known token (or on/off)",
    confidence: "Structural", severity: "Error" },

  { id: "button-name",
    wcag: "4.1.2", level: "A",
    title: "Each `<button>` must have an accessible name (text content or `aria-label`)",
    confidence: "Structural", severity: "Error" },

  { id: "caption-first-child",
    wcag: "1.3.1", level: "A",
    title: "A `<table>` `<caption>`, if present, must be the table's first child",
    confidence: "Structural", severity: "Error" },

  { id: "captions-live",
    wcag: "1.2.4", level: "AA",
    title: "Captions (Live): synchronised captions for all live synchronised-media audio",
    confidence: "Runtime", severity: "Info",
    applicability: "Live synchronised-media audio content.",
    expectation: "Real-time captions convey the live audio's speech and meaningful sounds." },

  { id: "character-key-shortcuts",
    wcag: "2.1.4", level: "A",
    title: "Character Key Shortcuts: single-character shortcuts can be turned off, remapped, or are active only on focus",
    confidence: "Runtime", severity: "Warning",
    applicability: "Keyboard shortcuts implemented using only letter, punctuation, number, or symbol characters.",
    expectation: "The shortcut can be disabled, remapped, or is active only while the relevant component has focus." },

  { id: "consistent-identification",
    wcag: "3.2.4", level: "AA",
    title: "Consistent Identification: components with the same functionality are identified consistently across pages",
    confidence: "Runtime", severity: "Info",
    applicability: "Components that have the same functionality within a set of web pages.",
    expectation: "They are identified consistently (same accessible name / label across the set)." },

  { id: "consistent-navigation",
    wcag: "3.2.3", level: "AA",
    title: "Consistent Navigation: repeated navigational mechanisms occur in the same relative order across pages",
    confidence: "Runtime", severity: "Info",
    applicability: "Navigational mechanisms repeated across a set of web pages.",
    expectation: "They occur in the same relative order each time, unless the user initiates a change." },

  { id: "content-on-hover-focus",
    wcag: "1.4.13", level: "AA",
    title: "Content on Hover or Focus: additional content triggered by hover/focus is dismissable, hoverable, and persistent",
    confidence: "Runtime", severity: "Warning",
    applicability: "Content that becomes visible on pointer hover or keyboard focus and then hidden again.",
    expectation: "The content is dismissable, hoverable, and persistent until dismissed or invalid." },

  { id: "contrast-minimum",
    wcag: "1.4.3", level: "AA",
    title: "Contrast (Minimum): text has a contrast ratio of at least 4.5:1 (3:1 for large text)",
    confidence: "Runtime", severity: "Warning",
    applicability: "Text and images of text rendered to the user.",
    expectation: "Contrast ratio is at least 4.5:1, or 3:1 for large-scale text; incidental and logotype text exempt." },

  { id: "document-lang",
    wcag: "3.1.1", level: "A",
    title: "The `<html>` root must carry a non-empty `lang` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "duplicate-id",
    wcag: "4.1.1", level: "A",
    title: "No two elements may share the same `id`",
    confidence: "Structural", severity: "Error" },

  { id: "error-identification",
    wcag: "3.3.1", level: "A",
    title: "Error Identification: input errors are detected and described to the user in text",
    confidence: "Runtime", severity: "Warning",
    applicability: "Form input errors that are automatically detected.",
    expectation: "The erroneous item is identified and the error described to the user in text." },

  { id: "error-prevention-legal",
    wcag: "3.3.4", level: "AA",
    title: "Error Prevention (Legal, Financial, Data): submissions are reversible, checked, or confirmable",
    confidence: "Runtime", severity: "Info",
    applicability: "Pages causing legal commitments, financial transactions, or data modification/deletion.",
    expectation: "Submissions are reversible, checked for errors, or confirmed before finalising." },

  { id: "error-suggestion",
    wcag: "3.3.3", level: "AA",
    title: "Error Suggestion: known correction suggestions are provided when an input error is detected",
    confidence: "Runtime", severity: "Info",
    applicability: "Input errors that are automatically detected and for which a correction is known.",
    expectation: "Correction suggestions are provided unless they would jeopardise security or purpose." },

  { id: "fieldset-legend",
    wcag: "1.3.1", level: "A",
    title: "Each `<fieldset>` should contain a `<legend>` as its accessible name",
    confidence: "Structural", severity: "Error" },

  { id: "focus-visible",
    wcag: "2.4.7", level: "AA",
    title: "Focus Visible: the keyboard focus indicator is visible",
    confidence: "Runtime", severity: "Warning",
    applicability: "User-interface components operable by keyboard.",
    expectation: "When a component has keyboard focus, a visible focus indicator is shown." },

  { id: "heading-no-skip",
    wcag: "1.3.1", level: "A",
    title: "Heading levels must not skip (e.g. h1 -> h3 disallowed)",
    confidence: "Structural", severity: "Error" },

  { id: "iframe-title",
    wcag: "4.1.2", level: "A",
    title: "Each `<iframe>` must have a non-empty `title` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "images-of-text",
    wcag: "1.4.5", level: "AA",
    title: "Images of Text: text is used in preference to images of text where the same presentation is possible",
    confidence: "Runtime", severity: "Info",
    applicability: "Images that render text (excluding logotypes and essential presentations).",
    expectation: "Real text is used instead of an image of text unless the presentation is essential or customisable." },

  { id: "img-alt",
    wcag: "1.1.1", level: "A",
    title: "Images must have an `alt` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "input-image-alt",
    wcag: "1.1.1", level: "A",
    title: "`<input type=\"image\">` must have a non-empty `alt` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "input-button-name",
    wcag: "4.1.2", level: "A",
    title: "`<input type=\"button|submit|reset\">` must have a `value` or `aria-label`/`title` accessible name",
    confidence: "Structural", severity: "Error" },

  { id: "keyboard",
    wcag: "2.1.1", level: "A",
    title: "Keyboard: all functionality is operable through a keyboard interface",
    confidence: "Runtime", severity: "Warning",
    applicability: "All interactive functionality of the page.",
    expectation: "Every function is operable via keyboard without requiring specific timings, except where the underlying task is path-dependent." },

  { id: "label-for-control",
    wcag: "1.3.1", level: "A",
    title: "Each `<label>` must have a `for` attribute or contain its control",
    confidence: "Structural", severity: "Error" },

  { id: "label-in-name",
    wcag: "2.5.3", level: "A",
    title: "Label in Name: the accessible name of a control contains its visible label text",
    confidence: "Runtime", severity: "Warning",
    applicability: "Components with labels that include text or images of text.",
    expectation: "The accessible name contains the text that is presented visually as the label." },

  { id: "link-empty-href",
    wcag: "2.4.4", level: "A",
    title: "`<a href=\"\">` is ineffective and rejected",
    confidence: "Structural", severity: "Error" },

  { id: "link-name",
    wcag: "2.4.4", level: "A",
    title: "Each `<a>` with `href` must have accessible text (text content or `aria-label`)",
    confidence: "Structural", severity: "Error" },

  { id: "meta-no-refresh",
    wcag: "2.2.1", level: "A",
    title: "`<meta http-equiv=\"refresh\">` triggers an unsolicited timeout",
    confidence: "Structural", severity: "Error" },

  { id: "motion-actuation",
    wcag: "2.5.4", level: "A",
    title: "Motion Actuation: functionality operated by device or user motion has a conventional control alternative",
    confidence: "Runtime", severity: "Info",
    applicability: "Functionality triggered by device motion (e.g. shaking) or user motion (gestures).",
    expectation: "An equivalent UI-component control exists and the motion response can be disabled." },

  { id: "no-empty-heading",
    wcag: "1.3.1", level: "A",
    title: "Heading elements (`<h1>`..`<h6>`) must have a non-empty accessible name",
    confidence: "Structural", severity: "Error" },

  { id: "no-keyboard-trap",
    wcag: "2.1.2", level: "A",
    title: "No Keyboard Trap: focus can be moved away from any component using only the keyboard",
    confidence: "Runtime", severity: "Warning",
    applicability: "Components that can receive keyboard focus.",
    expectation: "Focus can be moved away using standard keyboard navigation, or the user is told how." },

  { id: "non-text-contrast",
    wcag: "1.4.11", level: "AA",
    title: "Non-text Contrast: UI components and graphical objects have at least 3:1 contrast against adjacent colours",
    confidence: "Runtime", severity: "Warning",
    applicability: "User-interface component states and meaningful graphical objects.",
    expectation: "Contrast against adjacent colours is at least 3:1." },

  { id: "object-name",
    wcag: "1.1.1", level: "A",
    title: "Each `<object>` must have an accessible name (text content, `aria-label`, or `title`)",
    confidence: "Structural", severity: "Error" },

  { id: "orientation",
    wcag: "1.3.4", level: "AA",
    title: "Orientation: content does not restrict its view to a single display orientation unless essential",
    confidence: "Runtime", severity: "Info",
    applicability: "Content with an operable view orientation.",
    expectation: "Both portrait and landscape orientations are supported unless a specific orientation is essential." },

  { id: "pointer-cancellation",
    wcag: "2.5.2", level: "A",
    title: "Pointer Cancellation: single-pointer actions can be aborted or undone (no down-event execution)",
    confidence: "Runtime", severity: "Warning",
    applicability: "Functionality operable with a single pointer.",
    expectation: "No down-event activation, or completion is on up-event with abort/undo, or down-event is essential." },

  { id: "pointer-gestures",
    wcag: "2.5.1", level: "A",
    title: "Pointer Gestures: multipoint or path-based gestures have a single-pointer alternative",
    confidence: "Runtime", severity: "Warning",
    applicability: "Functionality using multipoint or path-based gestures.",
    expectation: "A single-pointer operation without a path-based gesture is also available, unless essential." },

  { id: "positive-tabindex",
    wcag: "2.4.3", level: "A",
    title: "Positive `tabindex` values disrupt natural focus order",
    confidence: "Heuristic", severity: "Warning" },

  { id: "reflow",
    wcag: "1.4.10", level: "AA",
    title: "Reflow: content reflows to a single column at 320px width without two-dimensional scrolling",
    confidence: "Runtime", severity: "Warning",
    applicability: "Content presented at a viewport width of 320 CSS px (400% zoom of 1280px).",
    expectation: "No loss of content or function and no two-dimensional scrolling, except where essential." },

  { id: "resize-text",
    wcag: "1.4.4", level: "AA",
    title: "Resize Text: text can be resized up to 200% without loss of content or functionality",
    confidence: "Runtime", severity: "Warning",
    applicability: "Text content (excluding captions and images of text).",
    expectation: "Resizing text up to 200% loses no content or functionality, without assistive technology." },

  { id: "select-has-options",
    wcag: "4.1.2", level: "A",
    title: "A `<select>` must contain at least one `<option>` to be operable",
    confidence: "Structural", severity: "Error" },

  { id: "status-messages",
    wcag: "4.1.3", level: "AA",
    title: "Status Messages: status messages are programmatically determinable without receiving focus",
    confidence: "Runtime", severity: "Warning",
    applicability: "Status messages that convey changes not associated with a focus change.",
    expectation: "They are exposed via role or property (e.g. a live region) so assistive tech announces them without moving focus." },

  { id: "summary-not-empty",
    wcag: "1.3.1", level: "A",
    title: "`<details>` must contain a non-empty `<summary>` for accessible name",
    confidence: "Structural", severity: "Error" },

  { id: "text-spacing",
    wcag: "1.4.12", level: "AA",
    title: "Text Spacing: no loss of content when line, paragraph, letter, and word spacing are overridden",
    confidence: "Runtime", severity: "Warning",
    applicability: "Text whose spacing can be set by the user via style overrides.",
    expectation: "No content or functionality is lost at line-height 1.5x, paragraph 2x, letter 0.12x, word 0.16x font size." },

  { id: "th-has-name",
    wcag: "1.3.1", level: "A",
    title: "Each table header cell (`<th>`) must have a non-empty accessible name",
    confidence: "Structural", severity: "Error" },

  { id: "th-scope-valid",
    wcag: "1.3.1", level: "A",
    title: "A `<th scope>` value must be one of row, col, rowgroup, colgroup",
    confidence: "Structural", severity: "Error" },

  { id: "track-kind",
    wcag: "1.2.2", level: "A",
    title: "Each `<track>` must declare a `kind` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "unique-main",
    wcag: "1.3.1", level: "A",
    title: "A document must contain at most one `<main>` landmark",
    confidence: "Structural", severity: "Error" },
];
