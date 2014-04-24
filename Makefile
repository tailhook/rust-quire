all: quire-lib

quire-lib: quire/mod.rs quire/*.rs
	rustc $<

test: quire_test
	./quire_test

quire_test: quire/mod.rs quire/*.rs
	rustc $< --test -o quire_test

.PHONY: test
