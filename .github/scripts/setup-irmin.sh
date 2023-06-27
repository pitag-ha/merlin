#!/bin/bash
set -eu

git clone https://github.com/mirage/irmin.git
cd irmin
git checkout 3.7.0
sudo apt install -y gnuplot-x11 libgmp-dev pkg-config libffi-dev
opam install . --deps-only --with-test
opam exec -- dune build @check
