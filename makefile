.ONESHELL:
export GIT_ROOT=$(shell git rev-parse --show-toplevel)
export OPAM=$(GIT_ROOT)/_external_bin/opam
export PATH := $(GIT_ROOT)/_external_bin:$(PATH)

# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh
#   Install external binaries in a dedicated directory
	mkdir -p $(GIT_ROOT)/_external_bin
	scripts/install_opam.sh

build-deps:
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	if [ -n "`opam switch show | grep -P ".+/ligo"`" ];
	then exit; else scripts/setup_dev_switch.sh;
	fi
#	Set up the local ligo opam repository so that it can be built
	if [ -n "`opam repo list --safe | grep -P "ligo-opam-repository"`" ];
	then exit; else scripts/setup_ligo_opam_repository.sh;
	fi
#	Install OCaml build dependencies for Ligo
	scripts/install_ligo_with_dependencies.sh

# Install editor tools to the ligo opam switch and setup editors config
# (Optionnal)
install-editor-tools: build-deps
	scripts/setup_editor_tools.sh

build: build-deps
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	scripts/test_ligo.sh