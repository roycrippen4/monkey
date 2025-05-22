(lang dune 3.18)

(name monkey)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name monkey)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  base
  stdio
  ppx_deriving
  ppx_import
  (ppx_assert :with-test)
  (ppx_inline_test :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
