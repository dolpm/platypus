.PHONY : all
all : platypus.native

platypus.native :
	opam exec -- \
	dune build

platypus.native.run : platypus.native
	./_build/install/default/bin/platypus

.PHONY : clean
clean :
	dune clean