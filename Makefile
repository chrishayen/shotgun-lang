PREFIX ?= $(HOME)/.local
OPAM_ENV = eval $$(opam env --switch=5.4.0) &&

.PHONY: build test clean install uninstall demo grammar

build:
	$(OPAM_ENV) cd bootstrap && dune build

test: build
	$(OPAM_ENV) cd bootstrap && dune test
	./tests/run_tests.sh

clean:
	cd bootstrap && dune clean
	rm -f *.c demo

install: build
	mkdir -p $(PREFIX)/bin
	cp bootstrap/_build/default/bin/main.exe $(PREFIX)/bin/shotgun

uninstall:
	rm -f $(PREFIX)/bin/shotgun

demo: build
	$(OPAM_ENV) dune exec --root bootstrap shotgun -- build examples/demo.bs -o demo
	./demo

grammar:
	cd tree-sitter-shotgun && npx tree-sitter generate
