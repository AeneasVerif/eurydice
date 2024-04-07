CHARON_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../charon
KRML_HOME 		?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../karamel
EURYDICE	?= ./eurydice $(EURYDICE_FLAGS)
CHARON		?= $(CHARON_HOME)/bin/charon

CHARON_TEST_FILES	= arrays
TEST_DIRS		= array const_generics traits array2d int_switch nested_arrays # step_by

.PHONY: all
all:
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "⚠️⚠️⚠️ Charon not found; we suggest cd lib && ln -s path/to/charon-ml charon"
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	dune build && ln -sf _build/default/bin/main.exe eurydice

CFLAGS		:= -Wall -Werror -Wno-unused-variable $(CFLAGS) -I$(KRML_HOME)/include

test: $(addprefix charon-test-,$(CHARON_TEST_FILES)) $(addprefix test-,$(TEST_DIRS))

# Tests relying on Charon's test infrastructure

$(CHARON_HOME)/tests/llbc/%.llbc: $(CHARON_HOME)/tests/src/%.rs
	RUST_BACKTRACE=1 $(MAKE) -C $(CHARON_HOME)/tests test-$*

.PRECIOUS: $(CHARON_HOME)/tests/llbc/%.llbc
charon-test-%: $(CHARON_HOME)/tests/llbc/%.llbc | out/test-% all
	mkdir -p out/charon-test-$*
	$(EURYDICE) --output out/charon-test-$* $<
	# These tests do not have a main
	cd out/charon-test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c -c

# Tests checked into the current repository
.PHONY: phony
.PRECIOUS: test/%/out.llbc
test/%/out.llbc: phony
	cd test/$* && $(CHARON) --errors-as-warnings && mv $*.llbc out.llbc

out/test-%/main.c: test/main.c
	mkdir -p out/test-$*
	sed 's/__NAME__/$*/g' $< > $@

test-const_generics_runtime: EXTRA=--const_generics runtime

test-%: test/%/out.llbc out/test-%/main.c | all
	$(EURYDICE) $(EXTRA) --output out/test-$* $<
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c main.c && ./a.out

.PRECIOUS: out/%
out/%:
	mkdir -p $@
