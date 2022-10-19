(** {1 \[\@\@deriving cad\]}

    A PPX deriver that generates functions for the spatial transformation of user defined
    abstract and record types composed of types for which said transformation functions
    are defined, in particular, the types of the {{:https://github.com/OCADml/OCADml}
    OCADml library} ({i e.g.} [V3.t] and [V2.t]), as well as CAD specific types such as
    [Scad.t] of {{:https://github.com/OCADml/OSCADml} OSCADml}. *)

(** [\[@@deriving cad\]]

    Derives [translate], [scale], [rotate], [axis_rotate], [quaternion], [affine], and
    [mirror] for the tagged type (along with xyz specific helpers) respecting the
    dimensionality (2D, 3D, or {{!page-index.intf_poly} either}) of the overall type as
    determined by presence of [OCADml] types or {{!page-index.dims} attribute tags}. *)
val cad : Ppxlib.Deriving.t

(** [\[@@deriving cad_jane\]]

    Same as [\[@@deriving cad\]], but defaults to expecting keyword [~f] parameters for
    {{!page-index.mappable_abstract} mappable} types other than [list], [option],
    [result], and {b tuples}. This can be overridden with the {{!page-index.cadmap}
    [\[@cad.map\]]} attribute. *)
val cad_jane : Ppxlib.Deriving.t
