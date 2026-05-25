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

  { id: "button-name",
    wcag: "4.1.2", level: "A",
    title: "Each `<button>` must have an accessible name (text content or `aria-label`)",
    confidence: "Structural", severity: "Error" },

  { id: "document-lang",
    wcag: "3.1.1", level: "A",
    title: "The `<html>` root must carry a non-empty `lang` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "duplicate-id",
    wcag: "4.1.1", level: "A",
    title: "No two elements may share the same `id`",
    confidence: "Structural", severity: "Error" },

  { id: "fieldset-legend",
    wcag: "1.3.1", level: "A",
    title: "Each `<fieldset>` should contain a `<legend>` as its accessible name",
    confidence: "Structural", severity: "Warning" },

  { id: "heading-no-skip",
    wcag: "1.3.1", level: "A",
    title: "Heading levels must not skip (e.g. h1 -> h3 disallowed)",
    confidence: "Structural", severity: "Error" },

  { id: "iframe-title",
    wcag: "4.1.2", level: "A",
    title: "Each `<iframe>` must have a non-empty `title` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "img-alt",
    wcag: "1.1.1", level: "A",
    title: "Images must have an `alt` attribute",
    confidence: "Structural", severity: "Error" },

  { id: "label-for-control",
    wcag: "1.3.1", level: "A",
    title: "Each `<label>` must have a `for` attribute or contain its control",
    confidence: "Structural", severity: "Error" },

  { id: "link-name",
    wcag: "2.4.4", level: "A",
    title: "Each `<a>` with `href` must have accessible text (text content or `aria-label`)",
    confidence: "Structural", severity: "Error" },

  { id: "unique-main",
    wcag: "1.3.1", level: "A",
    title: "A document must contain at most one `<main>` landmark",
    confidence: "Structural", severity: "Error" },
];
