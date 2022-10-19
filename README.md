# [@@deriving cad]
`ppx_deriving_cad` is a PPX deriver that generates functions for the spatial
transformation of user defined abstract and record types composed of types for
which said transformation functions are defined, in particular,
the types of the [OCADml library](https://github.com/OCADml/OCADml)
(*e.g.* `V3.t` and `V2.t`), as well as CAD specific types such as `Scad.t` of
[OSCADml](https://github.com/OCADml/OSCADml).

Documentation is available
[online](https://ocadml.github.io/ppx_deriving_cad/ppx_deriving_cad/index.html).
