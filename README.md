![oink](helloworld.png)

#

#### make commands:
##### install required packages &rarr; ```make install```
##### build the compiler &rarr; ```make```
##### run tests &rarr; ```make test```
##### run test &rarr; ```make test type=[ast|sast|codegen|compile] name=[test_name]```

#

#### compiler commands:
##### print AST &rarr; ```./platypus -a ./path/to/file.ppus```
##### print SAST &rarr; ```./platypus -a ./path/to/file.ppus```
##### print LLVM IR &rarr; ```./platypus -l ./path/to/file.ppus```
##### compile &rarr; ```./platypus -c ./path/to/file.ppus```
##### execute (JIT) &rarr; ```./platypus -e ./path/to/file.ppus```

#### extra flags:
```-k``` &rarr; persist intermediary files (```*.o```, ```*.bc```) generated during compilation
**usage:** ```./platypus -c -k ./test/test_cases/compile/pos_hello_world.ppus```

```-v``` &rarr; enable verbose compilation output
**usage:** ```./platypus -c -v ./test/test_cases/compile/pos_hello_world.ppus```
#

#### integration testing suite:
##### pos_hello_world &rarr; ```make test type=compile name=pos_hello_world```
- A very simple program test consisting of a platypus main function
which prints a string using the *printnl* function and exits. The desired output is the string being passed to *printnl*.

#

#### test validation:
For all test types, the testing program will validate the output of
a compiler command on a ```.ppus``` input file matches the expected output -- the desired output can be found in the ```.out``` file for each test.  For integration tests, we use llvm's executionengine to fetch this output (```-e```) to avoid the maintenance of auxiliary files during the usual compilation process (```-c```). If a program can't be compiled, the stderr output of the compiler command will be compared to the desired output.

#

#### references
- ```microc compiler```
- ```llvm.moe```

#

#### contact
- Dylan Maloy (dylan.maloy@tufts.edu)
- Rodrigo Campos (rodrigo.campos@tufts.edu)
- Ronit Sinha (ronit.sinha@tufts.edu)
- Tony Huang (ziheng.huang@tufts.edu)
- Abe Park (yangsun.park@tufts.edu)