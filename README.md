[![Build Status](https://travis-ci.org/TrueBitFoundation/ocaml-offchain.svg?branch=master)](https://travis-ci.org/TrueBitFoundation/ocaml-offchain)

# Usage

These instructions were tested on Ubuntu 17.04.

## Install
The easiest way to install the necessary dependencies is to run this script.
```
chmod 755 build.sh
./build.sh
```

## Run
```
chmod 755 run.sh
./run.sh -m *wast file*
```

For example:
```
./run.sh -m /test/core/fac.wast
```

## Test

```
cd interpreter/
./run_tests.sh
```

# Installation of the off-chain interpreter

Or you can follow along with these steps

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

# Docker

If you want to run these tests inside of a Docker container you can pull the latest docker image and try it out.

```
docker run --name tb-offchain -ti hswick/ocaml-offchain:latest`
cd webasm/interpreter
./wasm -t -m ../test/core/fac.wast
```

# License
The license for the test folder is the original Apache 2.0 license.<br/>
We have re-licensed the interpreter folder to MIT.<br/>
The license for the rest of the repo is MIT.<br/>
