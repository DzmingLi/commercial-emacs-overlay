EMACS ?= emacs

.PHONY: all check check-shr check-window-border check-glib-worker
all: lisp/gnus/commercial-gnus-loaddefs.el

lisp/gnus/commercial-gnus-loaddefs.el: $(filter-out lisp/gnus/commercial-gnus-loaddefs.el,$(wildcard lisp/gnus/*.el))
	$(EMACS) --batch -Q --eval '(progn (require (quote loaddefs-gen)) (loaddefs-generate "lisp/gnus" "lisp/gnus/commercial-gnus-loaddefs.el"))'

check: all
	$(EMACS) --batch -Q -L lisp/gnus -l tests/commercial-gnus-test.el -l tests/nnimap-scan-test.el -l tests/nnfeed-identity-test.el -f ert-run-tests-batch-and-exit

# SHR tests require an Emacs source tree with the overlay patch applied.
check-shr:
	$(EMACS) --batch -Q -l tests/shr-image-test.el -f ert-run-tests-batch-and-exit

check-window-border:
	$(EMACS) --batch -Q -l tests/window-border-test.el -f ert-run-tests-batch-and-exit

# EMACS must be the patched graphical build with WebKit on its load path.
# The isolated process and temporary state never use the user's daemon.
check-glib-worker:
	@set -eu; test_dir=$$(mktemp -d); \
	trap 'rm -rf "$$test_dir"' EXIT; \
	$(CC) -shared -fPIC -Wall -Wextra tests/glib-main-thread-probe.c \
	  -o "$$test_dir/probe.so" -ldl -pthread; \
	XDG_STATE_HOME="$$test_dir/state" LD_PRELOAD="$$test_dir/probe.so" \
	  timeout 30s $(EMACS) -Q -l tests/glib-worker-gui-test.el \
	  > "$$test_dir/log" 2>&1 || { cat "$$test_dir/log"; exit 1; }; \
	cat "$$test_dir/log"; \
	grep -q 'glib-worker-gui-test: PASS' "$$test_dir/log"; \
	grep -Eq 'glib-main-thread-probe: [1-9][0-9]* main-thread dispatches' "$$test_dir/log"
