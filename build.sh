sudo apt-get update

sudo apt-get install -y wget ocaml opam libzarith-ocaml-dev m4 pkg-config zlib1g-dev

opam init -y

eval `opam config env`
opam install cryptokit yojson -y

cd interpreter/
make