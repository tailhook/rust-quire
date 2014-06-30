LIBNAME := $(shell rustc --crate-file-name quire/mod.rs)


all: quire-lib quire_tool

quire-lib: $(LIBNAME)

$(LIBNAME): quire/mod.rs quire/*.rs
	rustc -g -o $@ $<

test: quire_test
	./quire_test

quire_test: quire/mod.rs quire/*.rs
	rustc $< --test -g -o quire_test

argparse:
	make -C rust-argparse argparse-lib

quire_tool: quire-tool.rs argparse quire-lib
	rustc $< -L . -L rust-argparse

.PHONY: test
