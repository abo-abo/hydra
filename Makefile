emacs ?= emacs

LOAD = -l lv.el -l hydra.el -l hydra-test.el

.PHONY: all test clean

all: test

test:
	@echo "Using $(shell which $(emacs))..."
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(emacs) -q $(LOAD) -l targets/hydra-init.el
	make clean

compile:
	$(emacs) -batch $(LOAD) -l targets/hydra-init.el

clean:
	rm -f *.elc
