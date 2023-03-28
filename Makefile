.PHONY : all
all : platypus.native

install : 
	opam install dune
	opam install llvm=14.0.6

platypus.native : clean
	opam exec -- dune build
	cp ./_build/install/default/bin/platypus ./

# TESTING RULE USAGE
#	to run entire test suite: make test
#
# 	to run a specific test: make test type=ast|sast name=NAME_OF_TEST
#		ex. make test type=ast name=pos_thing
#
#	to run all tests of a specific type: make test type=ast|sast
#		ex. make test type=sast

# Export env vars to be used by test shell scripty
test : export ttype = ${type}
test : export tname = ${name}
test : platypus.native
	opam exec -- dune test

# Usage: make file=NAME_OF_PPUS_FILE compile ; ./a
compile : platypus.native 
	./platypus -l $(file) > a.ll
	llvm-as a.ll -o a.bc
	llc -filetype=obj a.bc -o a.o
	clang a.o -no-pie -o a

zip : clean
	zip -r rodrigo_and_friends.zip . -x ".*"


.PHONY : clean
clean :
	dune clean
	rm -rf *.bc *.ll *.o
	rm -rf ./a
	rm -rf ./platypus
	rm -rf ./rodrigo_and_friends.zip