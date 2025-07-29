CHARON_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../charon
KRML_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../karamel
EURYDICE	?= ./eurydice $(EURYDICE_FLAGS)
CHARON		?= $(CHARON_HOME)/bin/charon

BROKEN_TESTS		= where_clauses println closure chunks mutable_slice_range issue_37 issue_99 issue_14
TEST_DIRS		= $(filter-out $(BROKEN_TESTS),$(basename $(notdir $(wildcard test/*.rs))))

# Warn on old versions of bash
_ := $(shell bash -c '(( $${BASH_VERSION%%.*} >= 4 ))')
ifneq ($(.SHELLSTATUS),0)
_: $(error "bash version is too old; hint: brew install bash")
endif

# Enable `foo/**` glob syntax
SHELL := bash -O globstar 

ifeq ($(shell uname -s),Darwin)
  ifeq (,$(shell which gsed))
    $(error gsed not found; try brew install gnu-sed)
  endif
  SED=gsed
else
  SED=sed
endif

ifneq ($(shell $(CHARON) version), $(shell cat .charon_version || true))
  _ := $(shell $(CHARON) version > .charon_version)
endif

.PHONY: all
all: format-check
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "⚠️⚠️⚠️ Charon not found; we suggest cd lib && ln -s path/to/charon charon"
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	$(MAKE) build

.PHONY: build
build:
	dune build && ln -sf _build/default/bin/main.exe eurydice

CFLAGS		:= -Wall -Werror -Wno-unused-variable $(CFLAGS) -I$(KRML_HOME)/include
CXXFLAGS	:= -std=c++17

test: $(addprefix test-,$(TEST_DIRS)) custom-test-array testxx-result

clean-and-test:
	$(MAKE) clean-llbc
	$(MAKE) test

.PRECIOUS: %.llbc
%.llbc: %.rs .charon_version
	# --mir elaborated --add-drop-bounds 
	$(CHARON) rustc --preset=eurydice --dest-file "$@" $(CHARON_EXTRA) -- $<

out/test-%/main.c: test/main.c
	mkdir -p out/test-$*
	sed 's/__NAME__/$*/g' $< > $@

test/issue_105.llbc: CHARON_EXTRA = \
  --include=core::result::*::branch \
  --include=core::result::*::from_residual \
  --include=core::result::*::eq \
  --include=core::cmp::* \
  --include=core::convert::*

test/array2d.llbc: CHARON_EXTRA = --include=core::array::equality::*

test/println.llbc: CHARON_EXTRA = \
  --include=core::fmt::Arguments --include=core::fmt::rt::*::new_const \
  --include=core::fmt::rt::Argument

test-partial_eq: EXTRA_C = ../../test/partial_eq_stubs.c
test-nested_arrays: EXTRA = -funroll-loops 0
test-array: EXTRA = -fcomments
test-symcrust: CFLAGS += -Wno-unused-function
test-more_str: EXTRA_C = ../../test/core_str_lib.c
test-more_primitive_types: EXTRA = --config test/more_primitive_types.yaml
test-global_ref: EXTRA_C = ../../test/core_cmp_lib.c


test-%: test/%.llbc out/test-%/main.c | all
	$(EURYDICE) $(EXTRA) --output out/test-$* $<
	$(SED) -i 's/  KaRaMeL version: .*//' out/test-$*/**/*.{c,h} # This changes on every commit
	$(SED) -i 's/  KaRaMeL invocation: .*//' out/test-$*/**/*.{c,h} # This changes between local and CI
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $(EXTRA_C) $*.c main.c && ./a.out

# C++ tests

out/testxx-%/main.cc: test/main.c
	mkdir -p out/testxx-$*
	sed 's/__NAME__/$*/g' $< > $@

testxx-%: test/%.llbc out/testxx-%/main.cc | all
	$(EURYDICE) $(EXTRA) -fc++17-compat --output out/testxx-$* $<
	$(SED) -i 's/  KaRaMeL version: .*//' out/testxx-$*/**/*.{c,h} # This changes on every commit
	$(SED) -i 's/  KaRaMeL invocation: .*//' out/testxx-$*/**/*.{c,h} # This changes between local and CI
	mv out/testxx-$*/$*.c out/testxx-$*/$*.cc
	cd out/testxx-$* && $(CXX) $(CXXFLAGS) $(CFLAGS) -I. -I../../include $(EXTRA_C) $*.cc main.cc && ./a.out

custom-test-array: test-array
	grep -q XXX1 out/test-array/array.c && \
	grep -q XXX2 out/test-array/array.c && \
	true

.PRECIOUS: out/%
out/%:
	mkdir -p $@

.PHONY: nix-magic
nix-magic:
	nix flake update karamel charon --extra-experimental-features nix-command --extra-experimental-features flakes

# Updates `flake.lock` with the latest commit from our local charon clone (the one that is symlinked into `lib/charon`).
.PHONY: update-charon-pin
update-charon-pin:
	nix-shell -p jq --run ./scripts/update-charon-pin.sh

FORMAT_FILE=include/eurydice_glue.h

.PHONY: format-check
format-check:
	@if ! dune build @fmt >/dev/null 2>&1; then \echo "\033[0;31m⚠️⚠️⚠️ SUGGESTED: $(MAKE) format-apply\033[0;m"; fi
	@F=$$(mktemp); clang-format $(FORMAT_FILE) > $$F; \
	  if ! diff -q $(FORMAT_FILE) $$F >/dev/null; then \echo "\033[0;31m⚠️⚠️⚠️ SUGGESTED: $(MAKE) format-apply\033[0;m"; fi; \
	  rm -rf $$F

.PHONY: format-check
format-apply:
	dune fmt >/dev/null || true
	clang-format -i $(FORMAT_FILE)

.PHONY: clean-llbc
clean-llbc:
	rm test/*.llbc || true
