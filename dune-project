(lang dune 3.16)

(name aoc_2023)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Gustavo Konrad")

(maintainers "Gustavo Konrad")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name aoc_2023)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base
  (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
