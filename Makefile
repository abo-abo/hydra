EMACS = emacs
# EMACS = emacs-24.3

LOAD = -l hydra.el -l hydra-test.el

.PHONY: all test clean

all: test

test:
	$(EMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q $(LOAD) -l init.el --eval "(progn (mapc #'byte-compile-file '(\"hydra.el\" \"init.el\")) (switch-to-buffer \"*Compile-Log*\") (ert t))"
	make clean

clean:
	rm -f *.elc
