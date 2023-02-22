.PHONY : all
all : platypus.native

platypus.native : clean
	opam exec -- dune build
	cp ./_build/install/default/bin/platypus ./

test : platypus.native
	opam exec -- dune test

.PHONY : clean
clean :
	dune clean
	rm -rf ./platypus