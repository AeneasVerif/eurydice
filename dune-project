(lang dune 3.7)

(name eurydice)

(generate_opam_files true)

(source
 (github AeneasVerif/eurydice))

(authors "Son Ho" "Jonathan Protzenko")

(maintainers "Son Ho" "Jonathan Protzenko")

(license Apache-2.0)

(package
 (name eurydice)
 (synopsis "A Rust to C translator")
 (description "Eurydice builds upon Charon to take existing Rust code and
              translate it to C")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
