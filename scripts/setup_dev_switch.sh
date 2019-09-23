#!/bin/sh
set -e

"$(dirname "$0")"/setup_switch.sh
"$(dirname "$0")"/setup_repos.sh

opam install -y ocp-indent tuareg merlin alcotest-lwt crowbar
opam -y user-setup install
