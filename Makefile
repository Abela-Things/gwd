-include Makefile.local

.DEFAULT_GOAL = build

build:
	dune build src/gwd.exe src/marshaler.exe

test:
	dune build @runtest

bench:
	dune build @runbench

clean:
	dune clean

.PHONY: clean
