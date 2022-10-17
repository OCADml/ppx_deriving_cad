(lang dune 3.2)

(name ppx_deriving_cad)

(generate_opam_files true)

(source
 (github OCADml/ppx_deriving_cad))

(documentation https://ocadml.github.io/ppx_deriving_cad/)

(license GPL-2.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com>")

(package
 (name ppx_deriving_cad)
 (synopsis "PPX Deriver for OCADml transformation functions")
 (description
  "[@@deriving cad] generates functions for the\nspatial transformation of user defined abstract and record types containing\ntypes for which said transformation functions are defined, in particular, the types of OCADml (and CAD backend specific implementations).")
 (depends
  (ocaml
   (>= 4.13.0))
  (base
   (and
    (>= 0.14.1)
    :with-test))
  (OCADml
   (and
    (<= 0.1.0)
    :with-test))
  (OSCADml
   (and
    (<= 0.1.0)
    :with-test))
  (ppxlib
   (>= 0.22.2))
  (ppx_inline_test :with-test)))
