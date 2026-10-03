EMACS ?= emacs

.PHONY: all check
all: lisp/gnus/commercial-gnus-loaddefs.el

lisp/gnus/commercial-gnus-loaddefs.el: $(filter-out lisp/gnus/commercial-gnus-loaddefs.el,$(wildcard lisp/gnus/*.el))
	$(EMACS) --batch -Q --eval '(progn (require (quote loaddefs-gen)) (loaddefs-generate "lisp/gnus" "lisp/gnus/commercial-gnus-loaddefs.el"))'

check: all
	$(EMACS) --batch -Q -L . -l commercial-gnus -l tests/commercial-gnus-test.el -f ert-run-tests-batch-and-exit
