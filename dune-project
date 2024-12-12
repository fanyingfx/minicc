(lang dune 3.17)

(name minicc)

(generate_opam_files true)

(map_workspace_root false)

(package
 (name minicc)
 (synopsis "Writing a C Compiler")
 (description
  "`nqcc2` is the reference implementation for the book *Writing a C Compiler*. It's a compiler for a subset of C, targeting x86-64 assembly.")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  bisect_ppx
  ppx_deriving
  ppx_expect
  ppx_inline_test
  cmdliner
  camlp-streams
  re))