all: libquire.rlib quire_tool


libquire.rlib: src/lib.rs src/*.rs
	rustc -g -o $@ $<

test: quire_test
	./quire_test

quire_test: src/lib.rs src/*.rs
	rustc $< --test -g -o quire_test

rust-argparse/libargparse.rlib:
	make -C rust-argparse libargparse.rlib

quire_tool: quire-tool.rs rust-argparse/libargparse.rlib libquire.rlib
	rustc $< -L . -L rust-argparse

.PHONY: test
