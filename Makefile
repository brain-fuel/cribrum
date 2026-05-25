.PHONY: test test-fast test-cribrum test-teaweb mutation ingest ingest-check build clean help

help:
	@echo "Targets:"
	@echo "  test         Run ingest-check + cribrum + teaweb suites + mutation gate"
	@echo "  test-fast    Run cribrum + teaweb suites (skip mutation)"
	@echo "  test-cribrum Cribrum suite only"
	@echo "  test-teaweb  TEAWeb suite only"
	@echo "  mutation     Mutation gate (changed-file scope; MUTATION_BASE=ALL for full)"
	@echo "  ingest       Regenerate Cribrum.Html.Model.Generated + Cribrum.AA.Catalog.Generated"
	@echo "  ingest-check Fail if either generated module drifts from its ingest source"
	@echo "  build        Typecheck cribrum + teaweb libraries"
	@echo "  clean        Remove build artifacts"

test: ingest-check test-cribrum test-teaweb mutation

test-fast: test-cribrum test-teaweb

test-cribrum:
	pack test cribrum_test

test-teaweb:
	pack test teaweb_test

mutation:
	test/mutation/run.sh

ingest:
	cd ingest && npm install --silent && npm run ingest

ingest-check:
	cd ingest && npm install --silent && npm run ingest:check

build:
	pack install cribrum
	pack install teaweb

clean:
	rm -rf build test/build test/teaweb/build
