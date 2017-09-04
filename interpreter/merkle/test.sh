#!/bin/sh

mkdir -p contracts
solc --abi --optimize --overwrite --bin -o contracts instruction.sol


