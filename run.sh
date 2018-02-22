#!/bin/bash
for i in $*;
do
  params=" $params $i"
done

./interpreter/wasm $params