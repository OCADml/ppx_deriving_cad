open Ppxlib

module type S = sig
  type t

  val unit : (t, unit) Attribute.t
  val ignore : (t, unit) Attribute.t
  val map : (t, unit) Attribute.t
  val mapf : (t, unit) Attribute.t
  val d2 : (t, unit) Attribute.t
  val d3 : (t, unit) Attribute.t
end

module M (T : sig
  type t

  val t : t Attribute.Context.t
end) =
struct
  type t = T.t

  let unit = Attribute.declare "cad.unit" T.t Ast_pattern.(pstr nil) ()
  let ignore = Attribute.declare "cad.ignore" T.t Ast_pattern.(pstr nil) ()
  let map = Attribute.declare "cad.map" T.t Ast_pattern.(pstr nil) ()
  let mapf = Attribute.declare "cad.mapf" T.t Ast_pattern.(pstr nil) ()
  let d2 = Attribute.declare "cad.d2" T.t Ast_pattern.(pstr nil) ()
  let d3 = Attribute.declare "cad.d3" T.t Ast_pattern.(pstr nil) ()
end

module Field : S with type t = label_declaration = M (struct
  type t = label_declaration

  let t = Attribute.Context.label_declaration
end)

module Type : S with type t = core_type = M (struct
  type t = core_type

  let t = Attribute.Context.core_type
end)

type t =
  { unit : bool
  ; ignored : bool
  ; jane : bool
  }

let get_unit = function
  | `Type ct -> Attribute.get Type.unit ct
  | `Field ld -> Attribute.get Field.unit ld

let get_ignore = function
  | `Type _ -> None
  | `Field ld -> Attribute.get Field.ignore ld

let get_map = function
  | `Type ct -> Attribute.get Type.map ct
  | `Field ld -> Attribute.get Field.map ld

let get_mapf = function
  | `Type ct -> Attribute.get Type.mapf ct
  | `Field ld -> Attribute.get Field.mapf ld

let update ~loc t kind =
  { unit = t.unit || (Option.is_some @@ get_unit kind)
  ; ignored = t.ignored || (Option.is_some @@ get_ignore kind)
  ; jane =
      ( match get_map kind, get_mapf kind with
      | None, None -> t.jane
      | None, Some () -> true
      | Some (), None -> false
      | Some (), Some () ->
        Location.raise_errorf ~loc "Cannot specify both map and mapf attributes." )
  }
