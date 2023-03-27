.PHONY : all
all : platypus.native

install : 
	opam install dune
	opam install llvm=14.0.6
	opam install ctypes-foreign

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
	find . -maxdepth 1 -type f -perm -ugo=x -delete
	rm -rf ./rodrigo_and_friends.zip