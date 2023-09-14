CHARON_HOME 	?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))/../charon
EURYDICE	= ./eurydice $(EURYDICE_FLAGS)

CHARON_TEST_FILES	= array
TEST_DIRS		= kyber768

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

charon-test-%: $(CHARON_HOME)/tests/llbc/%.llbc | out/test-% all
	$(EURYDICE) --output out/test-$* $<
	# These tests do not have a main
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c -c

# Tests checked into the current repository
test/%/out.llbc: | $(wildcard test/%/*.rs) all
	cd test/$* && $(CHARON_HOME)/bin/charon --crate $* --input lib.rs --no-code-duplication --dest .

test-%: test/%/out.llbc
	$(EURYDICE) --output out/test-$* $<
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c -c

.PRECIOUS: out/%
out/%:
	mkdir -p $@
