emacs ?= emacs
# emacs = emacs-24.3

LOAD = -l lv.el -l hydra.el -l hydra-test.el

.PHONY: all test clean

all: test

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(emacs) -q $(LOAD) -l hydra-init.el
	make clean

compile:
	$(emacs) -batch $(LOAD) -l hydra-init.el

clean:
	rm -f *.elc
