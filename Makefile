.PHONY : all
all : platypus.native

install : 
	opam install dune
	opam install llvm=14.0.6

platypus.native : clean
	opam exec -- dune build
	cp ./_build/install/default/bin/platypus ./

test : platypus.native
	opam exec -- dune test

zip : clean
	zip -r rodrigo_and_friends.zip . -x ".*"

.PHONY : clean
clean :
	dune clean
	rm -rf *.bc *.ll *.o
	rm -rf ./a
	rm -rf ./platypus
	rm -rf ./rodrigo_and_friends.zip