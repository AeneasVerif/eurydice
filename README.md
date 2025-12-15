<p><div style="text-align: center">
<img src="static/poussin.jpg"
     alt="Orphée et Eurydice" title="Orphée et Eurydice"
     style=""/>
<figcaption>
Nicolas Poussin, <i>Orphée et Eurydice</i>.
Musée du Louvre.
<a href="https://commons.wikimedia.org/wiki/File:Orph%C3%A9e_et_Eurydice_-_Nicolas_Poussin_-_Mus%C3%A9e_du_Louvre_Peintures_INV_7307.jpg">Source</a>
</figcaption>
</div></p>

# Eurydice

Eurydice is a compiler from Rust to C. The purpose of Eurydice is to provide a
backwards-compatibility story as the verification ecosystem gradually
transitions to Rust. New programs can be written in Rust, in turn making them
safer and easier to verify; but for legacy environments that cannot yet take a
dependency on the Rust toolchain, Eurydice allows generating C code as a stopgap
measure.

Currently (late 2023), the flagship example for Eurydice is Kyber, a
Post-Quantum cryptographic algorithm authored and
verified in Rust for the general public, and [compiled to C via
Eurydice](https://github.com/cryspen/hacl-packages/tree/7a7bfbb17d1d912bdb1a80e86a917e1eec8b6264/libcrux/src)
for Mozilla's NSS library.

In terms of software architecture, Eurydice consumes Rust programs via the
[Charon](https://github.com/AeneasVerif/charon) infrastructure, then extracts
Rust to [KaRaMeL](https://github.com/FStarLang/karamel)'s internal AST via a
type-driven translation. Once in the KaRaMeL AST, 30+ nano-passes allow going
from Rust down to C code. About half of these passes were already implemented
for KaRaMeL, the rest of the passes reuse the KaRaMeL infrastructure but were
freshly written for Eurydice.

If you want to contribute or ask questions, we strongly encourage you to join
the [Zulip](https://aeneas-verif.zulipchat.com/).

# Install

We recommend using Nix to easily ensure you are running the right versions of the tools and
libraries. With nix, you can run:
```bash
$ nix run 'github:aeneasverif/eurydice#charon' -- [CHARON_OPTIONS]
$ nix run 'github:aeneasverif/eurydice' -- [EURYDICE_OPTIONS] <llbc_file>
```

Alternatively, you can do a local setup as follows.

```bash
# Step 1: install OCaml environment. Follow instructions, reload your shell, and make sure 
# `eval $(opam env)` has been suitably added to your shell profile.
sudo apt install opam cargo # or brew on OSX
opam init

# Step 2: clone eurydice
git clone https://github.com/AeneasVerif/eurydice
cd eurydice

# Step 3: install dependent projects
# This will clone karamel, charon and libcrux. If you intend to also develop on one of these
# projects, you can symlink your working copy (e.g. `ln -s ../my-charon charon`) instead.
# Note: the invocation for karamel might fail, in which case you want to install all the packages
# in the `depends` field of karamel.opam except fstar. At the time of writing, this means typing:
# opam install ocamlfind batteries zarith stdint yojson ocamlbuild fileutils menhir pprint ulex process fix visitors wasm ppx_deriving ppx_deriving_yojson uucp
make setup-karamel
make setup-charon
make setup-libcrux

# Step 4: ready!
make test
```

# Submitting a successful PR

The C output of the test suite is under version control, and your PR will fail CI if running `make
test` generates a diff for the C files in `out/`. The reason for this is that we need to assess the
impact of a PR on the shape of the generated C code. To make sure the output of the tests is
up-to-date, you can run `make -B test` to force regeneration of the C files.

Our CI will also check that your OCaml and C files have proper formatting -- the target `make
format-apply` reformats your source code to conform to our style guide. It might sometimes be
difficult to have the exact right versions of ocamlformat and clang-format -- in case your PR still
fails, we recommend running `nix develop` followed by `make format-apply`.
