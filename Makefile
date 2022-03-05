.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune test

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
doc:
	dune build @doc
