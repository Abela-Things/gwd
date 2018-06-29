-include Makefile.local

.DEFAULT_GOAL = build

build:
	dune build src/gwd.exe src/marshaler.exe

clean:
	dune clean

.PHONY: clean
