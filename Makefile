-include Makefile.local

.DEFAULT_GOAL = build

build: piqi
	dune build src/gwd.exe src/marshaler.exe

test: piqi
	dune build @runtest

bench: piqi
	dune build @runbench

clean:
	dune clean

.PHONY: clean piqi
