.PHONY: test test-fast test-cribrum test-teaweb mutation build clean help

help:
	@echo "Targets:"
	@echo "  test         Run cribrum + teaweb suites + mutation gate"
	@echo "  test-fast    Run cribrum + teaweb suites (skip mutation)"
	@echo "  test-cribrum Cribrum suite only"
	@echo "  test-teaweb  TEAWeb suite only"
	@echo "  mutation     Mutation gate (changed-file scope; MUTATION_BASE=ALL for full)"
	@echo "  build        Typecheck cribrum + teaweb libraries"
	@echo "  clean        Remove build artifacts"

test: test-cribrum test-teaweb mutation

test-fast: test-cribrum test-teaweb

test-cribrum:
	pack test cribrum_test

test-teaweb:
	pack test teaweb_test

mutation:
	test/mutation/run.sh

build:
	pack install cribrum
	pack install teaweb

clean:
	rm -rf build test/build test/teaweb/build
