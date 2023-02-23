## platypus
A README describing how to compile and execute your compiler (as it stands), how to run your
test script, any syntax that you still need to add to your compiler, and the names and email ad-
dresses of all group members. If your compiler requires any packages on top of what MicroC re-
quires to be run, instructions on which packages to install and how to install them must be in-
cluded in the README.

platypus is a language designed to combine a safety-first ownership model and a developer friendly syntax. The intention is to enforce memory-safe practices at compilation, significantly reducing the runtime’s memory management overhead. Rust, the largest competitor in this domain, is a low-level systems language that can be difficult to understand for a user who is used to higher-level languages such as Python and JavaScript. Our goal is to act as an intermediary between these two parties.

### running the ast builder and pretty-printer
1. ```make```
2. ```./platypus -a ./path/to/file.ppus```

### running the tests
1. ```make test```

### required packages
1. 

### contact information
Dylan Maloy (dylan.maloy@tufts.edu)
Rodrigo Campos (rodrigo.campos@tufts.edu)
Ronit Sinha (ronit.sinha@tufts.edu)
Tony Huang (ziheng.huang@tufts.edu)
Abe Park (yangsun.park@tufts.edu)