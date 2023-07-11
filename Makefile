.PHONY: all
all:
	@ocamlfind list | grep -q charon || test -L lib/charon || echo "Charon not found; we suggest cd lib && ln -s path/to/charon-ml charon"
	dune build && ln -sf _build/default/bin/main.exe eurydice
