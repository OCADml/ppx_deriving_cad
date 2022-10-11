open Ppxlib
open! Ast_builder.Default

let is_constr = function
  | { ptyp_desc = Ptyp_constr _; _ } -> true
  | _ -> false

let option_map_expr ~loc expr =
  [%expr
    function
    | Some opt -> Some ([%e expr] opt)
    | None -> None]

let result_map_expr ~loc expr =
  [%expr
    function
    | Ok ok -> Ok ([%e expr] ok)
    | err -> err]

let list_map_expr ~loc expr =
  [%expr
    let rec aux acc = function
      | h :: t -> aux ([%e expr] h :: acc) t
      | [] -> acc
    in
    aux []]

let fun_id name lid =
  let maybe_suffix s =
    if String.equal s "t" then name else Printf.sprintf "%s_%s" name s
  in
  match lid with
  | Lident s -> Lident (maybe_suffix s)
  | Ldot (p, s) -> Ldot (p, maybe_suffix s)
  | Lapply _ -> assert false

let rec is_jane_map = function
  | Lident s -> String.equal "Map" s
  | Ldot (p, _) -> is_jane_map p
  | Lapply (a, b) -> is_jane_map a || is_jane_map b

let map_expr ~lid ~jane ~loc expr =
  let lid = if jane && is_jane_map lid then Longident.Ldot (lident "Map", "t") else lid in
  let id = pexp_ident ~loc { loc; txt = fun_id "map" lid } in
  if jane then [%expr [%e id] ~f:[%e expr]] else [%expr [%e id] [%e expr]]

let list_fold_result f init l =
  let rec aux acc = function
    | hd :: tl ->
      ( match f acc hd with
      | Ok res -> aux res tl
      | Error _ as e -> e )
    | [] -> Ok acc
  in
  aux init l
