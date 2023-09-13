CHARON_HOME 	?= ../charon
EURYDICE	= ./eurydice $(EURYDICE_FLAGS)

TEST_FILES	= array

.PHONY: all
all:
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "⚠️⚠️⚠️ Charon not found; we suggest cd lib && ln -s path/to/charon-ml charon"
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	dune build && ln -sf _build/default/bin/main.exe eurydice

test: $(addprefix test-,$(TEST_FILES))

$(CHARON_HOME)/tests/llbc/%.llbc: $(CHARON_HOME)/tests/src/%.rs
	RUST_BACKTRACE=1 $(MAKE) -C $(CHARON_HOME)/tests test-$*

CFLAGS		:= -Wall -Werror -Wno-unused-variable $(CFLAGS)

test-%: $(CHARON_HOME)/tests/llbc/%.llbc | out/test-% all
	$(EURYDICE) --output out/test-$* $<
	cd out/test-$* && $(CC) $(CFLAGS) -I. -I../../include $*.c -o $*.exe && ./$*.exe

.PRECIOUS: out/%
out/%:
	mkdir -p $@
