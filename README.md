# Installation of the off-chain interpreter

These instructions were tested on Ubuntu 17.04.

First install dependencies
```
apt-get install -y wget gcc ocaml opam libzarith-ocaml-dev m4 pkg-config zlib1g-dev
opam init -y
eval $(opam config env)
opam install cryptokit yojson
```

Then go to the `interpreter` directory of the repo
```
cd interpreter
make
```

This should generate the executable `wasm` in the `interpreter` directory.

# Testing the off-chain interpreter
```
./wasm -m ../test/core/fac.wast
```
If there are no errors, it means that the tests were passed.

```
./wasm -t -m ../test/core/fac.wast
```

This command will print the trace messages, it will basically output every instruction that the interpreter runs.

Outputting proofs:
```
./wasm -case 0 -step 4 -m ../test/core/fac.wast
```
This will make a proof for step 4 in the computation. Because there are many test cases, one of them has to be selected, so for example `-case 0` will select the first test case.

# WebAssembly spec README.md

This repository holds the sources for the WebAssembly draft specification
(to seed a future
[WebAssembly Working Group](https://lists.w3.org/Archives/Public/public-new-work/2017Jun/0005.html)),
a reference implementation, and the official testsuite.

A formatted version of the spec is available here:
[webassembly.github.io/spec](https://webassembly.github.io/spec/),

Participation is welcome. Discussions about new features, significant semantic
changes, or any specification change likely to generate substantial discussion
should take place in
[the WebAssembly design repository](https://github.com/WebAssembly/design)
first, so that this spec repository can remain focused. And please follow the
[guidelines for contributing](Contributing.md).
