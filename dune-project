(lang dune 3.15)

(name aoc2024)

(generate_opam_files true)

(source
 (github aazuspan/aoc))

(authors "Aaron Zuspan")

(package
 (name aoc2024)
 (synopsis "Advent of Code 2024 in OCaml")
 (depends ocaml dune ppx_deriving ppx_expect))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
