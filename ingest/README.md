# Cribrum spec ingestion

Per `plan.dj` §P2.1 / §P3.1: the HTML element model and the WCAG AA
rule catalog are large external specifications. Cribrum ingests them
from machine-readable upstreams rather than hand-transcribing them.

## html-model.ts

Pulls element data from [`@webref/elements`][webref] (the WHATWG /
W3C machine-readable element/attribute schema) and emits
`src/Cribrum/Html/Model/Generated.idr`.

```
cd ingest
npm install
npm run ingest         # regenerate Generated.idr
npm run ingest:check   # gate: error if Generated.idr is stale
```

### Round-trip discipline

The committed `src/Cribrum/Html/Model.idr` is the **authoritative**
catalog interpreted by the Phase-2 validator. The script's
`Generated.idr` output is the **regeneration target**: re-running the
script must produce a file equal to a previous run's output. CI is
expected to run `npm run ingest:check`; drift fails the gate.

The hand-curated `Cribrum.Html.Model` and the script's seed catalog
are currently synchronised manually. As `@webref/elements` matures its
content-model coverage, the script will compile the catalog directly
from upstream data rather than carrying the seed.

[webref]: https://github.com/w3c/webref

## aa-catalog.ts

Compiles `ingest/aa.ts` (typed staging area for the W3C ACT-rules pull
per plan §P3.1) into `src/Cribrum/AA/Catalog/Generated.idr`. The
auto-generated module exposes one `Rule` constant per row plus a
`generatedRules : List Rule` that `Cribrum.AA.Catalog` re-exports as
`allRules`.

```
cd ingest
npm install
npm run ingest:aa         # regenerate Generated.idr
npm run ingest:aa:check   # gate: error if Generated.idr is stale
```

The combined `npm run ingest` / `npm run ingest:check` run both the
HTML-model and AA-catalog gates back-to-back, and `make ingest-check`
is the project-level entry point.

### Scope today vs. plan §P3.1

`aa.ts` is hand-curated: id, WCAG SC, level, title, confidence,
severity — the existing `Rule` shape. The ACT-rules JSON pull (the
`applicability` / `expectation` data that would drive a true data
interpreter in `Cribrum.AA.Pass`) is the next slice; this scaffold
puts the I/O shape in place so the upstream pull lands as a content
change on `aa.ts` rather than a new pipeline.
