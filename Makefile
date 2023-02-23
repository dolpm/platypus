.PHONY : all
all : platypus.native

install : 
	opam install dune

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
	rm -rf ./platypus
	rm -rf ./rodrigo_and_friends.zip