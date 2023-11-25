(lang dune 3.4)

(using menhir 2.1)

(name herb)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name herb)
 (synopsis "Herb compiler")
 (description "A compiler for the herb programming language")
 (depends
  (ocaml
   (>= 5.1.0))
  (dune
   (>= 3.8.3))
  base-unix
  (base
   (>= 0.16.0))
  (stdio
   (>= 0.16.0))
  menhir
  cmdliner
  fmt
  logs
  ppx_deriving
  location)
 (tags
  (topics "to describe" your project)))

(package
 (name ollvm)
 (depends
  (ocaml
   (>= 5.1.0))
  (dune
   (>= 3.8.3))
  menhir))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
