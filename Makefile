.PHONY: test check

# source: https://stackoverflow.com/questions/24005166/gnu-make-silent-by-default
ifndef VERBOSE
.SILENT:
endif

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc docs:
	dune build @doc

cloc:
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .

bisect test: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe -- -runner sequential
	bisect-ppx-report html
	echo Coverage report saved to: $(PWD)/_coverage/index.html

bisect-clean:
	rm -rf _coverage bisect*.coverage

view-test view-bisect:
	open -na "Brave Browser Beta" --args --incognito $(PWD)/_coverage/index.html

zip:
	rm -f octet.zip
	zip -r octet.zip . -x@exclude.lst
