all: quire-lib

quire-lib: quire/mod.rs quire/*.rs
	rustc -g $<

test: quire_test
	./quire_test

quire_test: quire/mod.rs quire/*.rs
	rustc $< --test -g -o quire_test

.PHONY: test
