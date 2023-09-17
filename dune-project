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
 (depends ocaml dune base-unix base stdio menhir cmdliner fmt logs ppx_deriving location ollvm) 
 
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
