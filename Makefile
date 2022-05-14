.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/octet.exe -- -runner sequential

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc docs:
	dune build @doc

cloc:
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/octet.exe -- -runner sequential
	bisect-ppx-report html
	open -na "Brave Browser Beta" --args --incognito $(PWD)/_coverage/index.html 

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f octet.zip
	zip -r octet.zip . -x@exclude.lst
