CHARON_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../charon
EURYDICE	= ./eurydice $(EURYDICE_FLAGS)

CHARON_TEST_FILES	= array
TEST_DIRS		= kyber768 array const_generics traits array2d int_switch # step_by 

.PHONY: all
all:
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "⚠️⚠️⚠️ Charon not found; we suggest cd lib && ln -s path/to/charon-ml charon"
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	dune build && ln -sf _build/default/bin/main.exe eurydice

CFLAGS		:= -Wall -Werror -Wno-unused-variable $(CFLAGS)

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
test/%/out.llbc: phony
	cd test/$* && $(CHARON_HOME)/bin/charon && mv $*.llbc out.llbc

test-%: test/%/out.llbc | all
	mkdir -p out/test-$*
	$(EURYDICE) --output out/test-$* $<
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c -c

.PRECIOUS: out/%
out/%:
	mkdir -p $@
