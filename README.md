![oink](logo.png)

#

```c
pipe main [] |> unit {
  |> Printnl <| ["hello, world!"];
}
```


Platypus is a language designed to combine a safety-first ownership model and a developer friendly syntax. The intention is to enforce memory-safe development practices *and* perform all necessary memory management at compile time.

Rust, the largest presence in this domain, is a low-level systems language that can be difficult to understand for a user who is used to higher-level ones. The goal of Platypus is to act as an intermediary between these two.

#

#### installation
##### install deps &rarr; ```make install```
##### compile &rarr; ```make```

#

#### compiler commands
##### print AST &rarr; ```./platypus -a ./path/to/file.ppus```
##### print SAST &rarr; ```./platypus -s ./path/to/file.ppus```
##### print LLVM IR &rarr; ```./platypus -l ./path/to/file.ppus```
##### execute (JITc) &rarr; ```./platypus -e ./path/to/file.ppus```
##### compile into executable &rarr; ```./platypus -c ./path/to/file.ppus```

#

#### optional flags
```-k``` &rarr; persist intermediary files (```*.o```, ```*.bc```) generated during compilation

**usage:** ```./platypus -c -k ./test/test_cases/compile/pos_hello_world.ppus```

```-v``` &rarr; enable verbose compilation output

**usage:** ```./platypus -c -v ./test/test_cases/compile/pos_hello_world.ppus```

```-o``` &rarr; enable compiler optimizations

**usage:** ```./platypus -c -o ./test/test_cases/compile/pos_hello_world.ppus```

#

#### code samples
check out some examples [here](https://github.com/dolpm/platypus/tree/main/examples)!

#

#### testing
##### run all &rarr; ```make test [memcheck=true]```
##### run single &rarr; ```make test type=[ast|sast|codegen|compile] name=[test_name] [memcheck=true]```

#

#### testing mechanisms
For all test types, the testing program will validate the output of
a compiler command on a ```.ppus``` input file matches the expected output -- the desired output can be found in the ```.out``` file for each test.  For integration tests, we use llvm's executionengine to fetch this output (```-e```) to avoid the maintenance of auxiliary files during the usual compilation process (```-c```). If a program can't be compiled, the stderr output of the compiler command will be compared to the desired output.

#

#### more info
for a mroe in-depth overview of the language, its features, and implementation, take a look at the [LRM](https://github.com/dolpm/platypus/tree/main/platypus.pdf)!

#
