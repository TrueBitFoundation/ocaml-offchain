#!/bin/zsh

node ~/wasdk/dist/wasdk.js ez filesystem.json
./wasm -merge ~/ocamlrun-wasm/build/ocamlrun.wasm filesystem.wasm

./wasm -m -t -file filesystem.wasm -table-size 1024 -memory-size $[16*1024*2] -wasm merge.wasm

