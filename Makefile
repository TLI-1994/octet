.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/octet.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
doc:
	dune build @doc

cloc:
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .
