#!/bin/sh
set -e
set -x

opam update

#pin bls12 until Danny makes a new tag/release
opam pin add bls12-381.1.1.0 git+https://gitlab.com/dannywillems/ocaml-bls12-381.git#e09af447e3a6757f490160e2c87578b1731dc786 --no-action
opam pin add bls12-381-unix.1.1.0 git+https://gitlab.com/dannywillems/ocaml-bls12-381.git#e09af447e3a6757f490160e2c87578b1731dc786 --no-action

opam install -y ctypes-foreign

# Install local dependencies
opam install -y --deps-only --with-test ./ligo.opam --locked
