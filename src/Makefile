.PHONY : all
all : platypus.native

platypus.native : parser.mly scanner.mll platypus.ml
	opam exec -- \
	ocamlbuild -use-ocamlfind platypus.native

.PHONY : clean
clean :
	ocamlbuild -clean