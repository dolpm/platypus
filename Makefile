.PHONY : all
all : platypus.native

docker.build :
	@docker volume prune -f
	docker build --no-cache -q -t ghcr.io/dolpm/platypus:latest .
	@docker image prune -f

docker.run :
	docker run --rm -it -v platypus:/app/ -w=/app/ ghcr.io/dolpm/platypus:latest

docker.push : 
	docker push ghcr.io/dolpm/platypus:latest

install : 
	opam install --confirm-level=unsafe-yes dune
	opam install --confirm-level=unsafe-yes llvm=14.0.6
	opam install --confirm-level=unsafe-yes ctypes-foreign

platypus.native : clean
	@dune build
	@cp ./_build/install/default/bin/platypus ./

# TESTING RULE USAGE
#	to run entire test suite: make test
#
# 	to run a specific test: make test type=ast|sast|codegen|compile name=NAME_OF_TEST
#		ex. make test type=ast name=pos_thing
#
#	to run all tests of a specific type: make test type=ast|sast|codegen|compile
#		ex. make test type=sast

# Export env vars to be used by test shell scripty
test : export ttype = ${type}
test : export tname = ${name}
test : export tmemcheck = ${memcheck}
test : platypus.native
	@dune test

zip : clean
	@zip -r rodrigo_and_friends.zip . -x ".*"

.PHONY : clean
clean :
	@dune clean
	@find . -maxdepth 1 -type f -perm -ugo=x -delete
	@rm -rf *.bc *.o
	@rm -rf ./rodrigo_and_friends.zip