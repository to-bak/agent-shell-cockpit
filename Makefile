EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch --eval '(setq load-prefer-newer t)'
STRAIGHT_BUILD ?= $(HOME)/.emacs.d/straight/build
DEPENDENCY_NAMES = agent-shell shell-maker acp magit-section transient compat cond-let llama seq dash map package-lint
DEPENDENCY_DIRS = $(foreach name,$(DEPENDENCY_NAMES),$(wildcard $(STRAIGHT_BUILD)/$(name) $(STRAIGHT_BUILD)/$(name)-[0-9]*))
EXTRA_LOAD_PATH ?=
DEPENDENCY_LOAD_PATH = $(EXTRA_LOAD_PATH) $(foreach dir,$(DEPENDENCY_DIRS),-L $(dir))
PACKAGE_FILES = $(wildcard agent-shell-cockpit*.el)

.PHONY: all check test integration optional-integration smoke compile checkdoc lint package clean

all: check

check: test integration compile checkdoc smoke

test:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . -L test \
	  -l agent-shell-cockpit-test-helper \
	  -l agent-shell-cockpit-store-test \
	  -l agent-shell-cockpit-git-test \
	  -l agent-shell-cockpit-instructions-test \
	  -l agent-shell-cockpit-session-ui-test \
	  -l agent-shell-cockpit-regression-test \
	  -l agent-shell-cockpit-lifecycle-test \
	  -f ert-run-tests-batch-and-exit

integration:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . -L test \
	  -l agent-shell-cockpit-integration-test -l agent-shell-cockpit-safety-test -f ert-run-tests-batch-and-exit

optional-integration:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) $(foreach name,evil goto-chg org-roam emacsql,$(foreach dir,$(wildcard $(STRAIGHT_BUILD)/$(name) $(STRAIGHT_BUILD)/$(name)-[0-9]*),-L $(dir))) -L . \
	  -l test/agent-shell-cockpit-optional-test.el -f ert-run-tests-batch-and-exit

smoke:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . -l test/package-smoke.el

compile:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(PACKAGE_FILES)

checkdoc:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  -l test/checkdoc-runner.el \
	  $(PACKAGE_FILES)

lint:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  -l package-lint -l test/lint-dependencies.el $(PACKAGE_FILES)

package:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . -l test/build-package.el

clean:
	$(RM) *.elc test/*.elc
