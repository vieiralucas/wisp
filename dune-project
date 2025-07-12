(lang dune 3.19)

(name wisp)

(generate_opam_files true)

(source
 (github vieiralucas/wisp))

(authors "Lucas Vieira <vieiralucas4@gmail.com>")

(maintainers "Lucas Vieira <vieiralucas4@gmail.com>")

(license MIT)

(documentation https://github.com/vieiralucas/wisp)

(package
 (name wisp)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml eio_main alcotest ocamlformat ppx_deriving uri)
 (tags
  (http web server)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
