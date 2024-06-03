CHARON_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../charon
KRML_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../karamel
EURYDICE	?= ./eurydice $(EURYDICE_FLAGS)
CHARON		?= $(CHARON_HOME)/bin/charon

BROKEN_TESTS		= step_by where_clauses chunks mutable_slice_range
TEST_DIRS		= $(filter-out $(BROKEN_TESTS),$(subst test/,,$(shell find test -maxdepth 1 -mindepth 1 -type d)))

.PHONY: all
all:
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "⚠️⚠️⚠️ Charon not found; we suggest cd lib && ln -s path/to/charon-ml charon"
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	dune build && ln -sf _build/default/bin/main.exe eurydice

CFLAGS		:= -Wall -Werror -Wno-unused-variable $(CFLAGS) -I$(KRML_HOME)/include

test: $(addprefix test-,$(TEST_DIRS))

.PHONY: phony
.PRECIOUS: test/%/out.llbc
test/%/out.llbc: phony
	cd test/$* && $(CHARON) --errors-as-warnings && mv $*.llbc out.llbc

out/test-%/main.c: test/main.c
	mkdir -p out/test-$*
	sed 's/__NAME__/$*/g' $< > $@

test-partial_eq: EXTRA_C = ../../test/partial_eq/stubs.c
test-nested_arrays: EXTRA = -funroll-loops 0

test-%: test/%/out.llbc out/test-%/main.c | all
	$(EURYDICE) $(EXTRA) --output out/test-$* $<
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $(EXTRA_C) $*.c main.c && ./a.out

.PRECIOUS: out/%
out/%:
	mkdir -p $@

nix-magic:
	nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes
