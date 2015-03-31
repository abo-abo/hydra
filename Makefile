emacs ?= emacs
# emacs = emacs-24.3

LOAD = -l lv.el -l hydra.el -l hydra-test.el

.PHONY: all test clean

all: test

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -q $(LOAD) -l hydra-init.el
	make clean

clean:
	rm -f *.elc
