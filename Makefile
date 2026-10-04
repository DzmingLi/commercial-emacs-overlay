EMACS ?= emacs

.PHONY: all check check-shr check-window-border
all: lisp/gnus/commercial-gnus-loaddefs.el

lisp/gnus/commercial-gnus-loaddefs.el: $(filter-out lisp/gnus/commercial-gnus-loaddefs.el,$(wildcard lisp/gnus/*.el))
	$(EMACS) --batch -Q --eval '(progn (require (quote loaddefs-gen)) (loaddefs-generate "lisp/gnus" "lisp/gnus/commercial-gnus-loaddefs.el"))'

check: all
	$(EMACS) --batch -Q -L lisp/gnus -l tests/commercial-gnus-test.el -f ert-run-tests-batch-and-exit

# SHR tests require an Emacs source tree with the overlay patch applied.
check-shr:
	$(EMACS) --batch -Q -l tests/shr-image-test.el -f ert-run-tests-batch-and-exit

check-window-border:
	$(EMACS) --batch -Q -l tests/window-border-test.el -f ert-run-tests-batch-and-exit
