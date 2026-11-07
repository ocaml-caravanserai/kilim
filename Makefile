# The targets:
#
#   make build          Build all.
#   make fmt            Format the code.
#   make test           Run the tests: native code, js_of_ocaml and
#                       wasm_of_ocaml (on node, and in Chrome for test/dom).
#   make size           Build the counter of bench/size in the release
#                       profile, and compare its gzip size with
#                       bench/size/budget (docs/budgets.md).
#   make test-release   Run the loader tests of the release files in Chrome
#                       (test/loader: kilim dist on the release builds).
#   make mutate         Test the mutants on the lines that changed since BASE.
#   make mutate-all     Test all the mutants of the libraries.
#   make mutate-report  Merge the verdicts of the last run of mutate or
#                       mutate-all, and list the survivors on the lines that
#                       changed since BASE. Give BASE= for all the lines.
#   make mutate-arm MUTANT=src/core/cancel.ml:40:12:skip
#                       Run SUITE with one mutant armed.
#
# The variables:
#
#   BASE       The git revision of the changed lines. The default is the
#              merge base with origin/main.
#   JOBS       The number of mutants that each suite tests at the same time
#              (default auto: one for each CPU).
#   MIN_SCORE  If it is set, the report gives exit status 1 only when the
#              score is less than MIN_SCORE, and not for each survivor.
#   SUITE      The suite of mutate-arm (default test/core/test_core.exe).
#
# Outside the dev shell, each command runs in `nix develop`. Mutation testing
# runs only the native suites: windtrap has C stubs, thus it does not run
# under js_of_ocaml or wasm_of_ocaml.

BASE ?= $(shell git merge-base origin/main HEAD)
JOBS ?= auto
MIN_SCORE ?=
SUITE ?= test/core/test_core.exe
MUTANT ?=

DUNE := $(if $(IN_NIX_SHELL),dune,nix develop -c dune)
MUTATE := --instrument-with ppx_windtrap.mutate

# The suites of the libraries. test/core is the suite of Async and Cancel.
# test/view is the suite of the view, the diff and the fake DOM of
# kilim.testing. test/turn is the suite of the commands, the app, the turn
# and the virtual host of kilim.testing. test/dist is the suite of kilim_dist
# (src/loader: the executable kilim).
SUITES := test/core test/view test/turn test/dist

report = $(DUNE) exec $(MUTATE) windtrap -- mutants \
  $(if $(1),--since $(1)) $(if $(MIN_SCORE),--min-score $(MIN_SCORE))

.PHONY: help build fmt test size test-release check-windtrap mutate mutate-all mutate-report mutate-arm

help:
	@sed -n '1,/^$$/p' Makefile | sed 's/^# \{0,1\}//'

build:
	$(DUNE) build

fmt:
	$(DUNE) fmt

test:
	$(DUNE) test --force

size:
	$(DUNE) build --force @bench/size/size --profile release

test-release:
	$(DUNE) build --force @test/loader/runtest --profile release

# A dev shell that started before a change of flake.nix keeps its old
# windtrap, and an old windtrap has no --since.
check-windtrap:
	@$(DUNE) exec $(MUTATE) windtrap -- mutants --help 2>/dev/null | grep -q -- --since || { \
	  echo "The windtrap of this shell is not the fork of flake.nix."; \
	  echo "Start nix develop again, or run direnv reload."; exit 2; }

# A suite with a survivor gives exit status 1, so the run ignores the status,
# and the report gives the result.
define run_suites
-WINDTRAP_MUTATE=1 $(if $(1),WINDTRAP_MUTATE_SINCE=$(1)) WINDTRAP_MUTATE_JOBS=$(JOBS) \
  $(DUNE) runtest --force $(MUTATE) $(SUITES)
endef

# The build comes first, so that a compile error stops the target. The report
# gives a failure only for a survivor: a mutant that no test reaches gives no
# failure. The CI job `mutation` runs this target on each pull request.
mutate: check-windtrap
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,$(BASE))
	$(call report,$(BASE))

mutate-all: check-windtrap
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,)
	$(call report,)

mutate-report: check-windtrap
	$(call report,$(BASE))

mutate-arm:
	@test -n "$(MUTANT)" || { echo "Give MUTANT, for example MUTANT=src/core/cancel.ml:40:12:skip"; exit 2; }
	$(DUNE) exec $(MUTATE) $(SUITE) -- --arm $(MUTANT)
