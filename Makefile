EMACS = emacs
# EMACS = emacs-24.3

.PHONY: all test clean

all: test

test:
	$(EMACS) -batch -l hydra.el -l hydra-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
