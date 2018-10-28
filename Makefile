emacs ?= emacs
LOAD = -l cardan-utils.el -l cardan-slots.el -l cardan-saved-addresses.el -l cardan.el

.PHONY: all test clean

all: test

test:
	@echo "Using $(shell which $(emacs))"
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(emacs) -q $(LOAD) -l targets/cardan-init.el
	make clean

compile:
	$(emacs) -batch $(LOAD) -l targets/cardan-init.el

clean:
	rm -f *.elc
