EMACS ?= emacs

.PHONY: check check-window-border
check: check-window-border

# Use an Emacs binary built with patches/gnu-emacs31-window-border.patch.
# GUI geometry tests run when the test process has a graphical frame.
check-window-border:
	$(EMACS) --batch -Q -l tests/window-border-test.el -f ert-run-tests-batch-and-exit
