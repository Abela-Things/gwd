-include Makefile.local

.DEFAULT_GOAL = build

ifdef PROFILE
    DUNE_PROFILE = --profile $(PROFILE)
endif

build: piqi
	dune build $(DUNE_PROFILE) src/gwd.exe src/marshaler.exe

test: piqi
	dune build $(DUNE_PROFILE) @runtest

bench: piqi
	dune build $(DUNE_PROFILE) @runbench

clean:
	dune clean

.PHONY: clean piqi
