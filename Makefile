EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch --eval '(setq load-prefer-newer t)'
STRAIGHT_BUILD ?= $(HOME)/.emacs.d/straight/build
DEPENDENCY_DIRS = $(filter-out $(STRAIGHT_BUILD)/agent-shell-cockpit,$(wildcard $(STRAIGHT_BUILD)/*))
DEPENDENCY_LOAD_PATH = $(foreach dir,$(DEPENDENCY_DIRS),-L $(dir))
PACKAGE_FILES = $(wildcard agent-shell-cockpit*.el)

.PHONY: all check test compile checkdoc lint clean

all: check

check: test compile checkdoc

test:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . -L test \
	  -l agent-shell-cockpit-test-helper \
	  -l agent-shell-cockpit-store-test \
	  -l agent-shell-cockpit-git-test \
	  -l agent-shell-cockpit-session-ui-test \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(PACKAGE_FILES)

checkdoc:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  --eval '(require (quote checkdoc))' \
	  --eval '(mapc (lambda (file) (checkdoc-file file)) command-line-args-left)' \
	  $(PACKAGE_FILES)

lint:
	$(EMACS_BATCH) $(DEPENDENCY_LOAD_PATH) -L . \
	  -l package-lint \
	  -f package-lint-batch-and-exit agent-shell-cockpit.el

clean:
	$(RM) *.elc test/*.elc
