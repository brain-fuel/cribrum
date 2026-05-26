.PHONY: test test-fast test-cribrum test-teaweb djotref djotref-update djotref-ingest oracle mutation ingest ingest-check build clean readme docsnav help

help:
	@echo "Targets:"
	@echo "  test           Run ingest-check + cribrum + teaweb suites + djotref + mutation gate"
	@echo "  test-fast      Run cribrum + teaweb suites (skip djotref + mutation)"
	@echo "  test-cribrum   Cribrum suite only"
	@echo "  test-teaweb    TEAWeb suite only"
	@echo "  djotref        Djot reference-suite gate (regresses on baseline break)"
	@echo "  djotref-update Refresh test/djot-ref/baseline.txt with current pass set"
	@echo "  djotref-ingest Re-download jgm/djot.lua reference corpus into test/djot-ref/corpus/"
	@echo "  oracle         Cross-check Cribrum's decideHtml against vnu.jar (W3C HTML validator)"
	@echo "  mutation       Mutation gate (changed-file scope; MUTATION_BASE=ALL for full)"
	@echo "  ingest         Regenerate Cribrum.Html.Model.Generated + Cribrum.AA.Catalog.Generated"
	@echo "  ingest-check   Fail if either generated module drifts from its ingest source"
	@echo "  build          Typecheck cribrum + teaweb libraries"
	@echo "  readme         Render README.dj -> README.html via the actual Cribrum pipeline"
	@echo "  docsnav        Regenerate examples/teaweb/docsnav/{index.html,src/Generated.idr} from README.dj, then JS-build the island"
	@echo "  clean          Remove build artifacts"

test: ingest-check test-cribrum test-teaweb djotref mutation

test-fast: test-cribrum test-teaweb

test-cribrum:
	pack test cribrum_test

test-teaweb:
	pack test teaweb_test

djotref:
	pack -q run tools/run-djotref/run-djotref.ipkg

djotref-update:
	pack -q run tools/run-djotref/run-djotref.ipkg --update

djotref-ingest:
	cd ingest && npm install --silent && npm run ingest:djotref

# P2.4 oracle: oracle-emit emits the JSONL corpus on stdout (with a
# diagnostic line on stderr), oracle.ts validates each HTML against
# vnu.jar and reports any (cribrum, expected, vnu) divergence.
oracle:
	cd ingest && npm install --silent
	pack -q run tools/oracle-emit/oracle-emit.ipkg \
	  | (cd ingest && npx tsx oracle.ts)

mutation:
	test/mutation/run.sh

ingest:
	cd ingest && npm install --silent && npm run ingest

ingest-check:
	cd ingest && npm install --silent && npm run ingest:check

build:
	pack install cribrum
	pack install teaweb

readme:
	pack run tools/render-doc/render-doc.ipkg README.dj README.html "Cribrum"

docsnav:
	pack run tools/render-docsnav/render-docsnav.ipkg \
	    README.dj \
	    examples/teaweb/docsnav/index.html \
	    examples/teaweb/docsnav/src/Generated.idr
	pack --cg javascript build examples/teaweb/docsnav/docsnav.ipkg

clean:
	rm -rf build test/build test/teaweb/build tools/render-doc/build tools/render-docsnav/build tools/run-djotref/build examples/teaweb/docsnav/build
