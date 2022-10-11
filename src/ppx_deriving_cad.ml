open! Ppxlib
open! Ast_builder.Default

type transform =
  | Translate
  | Xtrans
  | Ytrans
  | Ztrans
  | Rotate
  | Xrot
  | Yrot
  | Zrot
  | Quaternion
  | AxisRotate
  | Affine
  | Scale
  | Xscale
  | Yscale
  | Zscale
  | Mirror

let transforms_2d =
  [ Translate; Xtrans; Ytrans; Rotate; Zrot; Scale; Xscale; Yscale; Mirror; Affine ]

let transforms_3d =
  [ Translate
  ; Xtrans
  ; Ytrans
  ; Ztrans
  ; Rotate
  ; Xrot
  ; Yrot
  ; Zrot
  ; AxisRotate
  ; Quaternion
  ; Scale
  ; Xscale
  ; Yscale
  ; Zscale
  ; Mirror
  ; Affine
  ]

let transform_to_string = function
  | Translate -> "translate"
  | Xtrans -> "xtrans"
  | Ytrans -> "ytrans"
  | Ztrans -> "ztrans"
  | Scale -> "scale"
  | Xscale -> "xscale"
  | Yscale -> "yscale"
  | Zscale -> "zscale"
  | Rotate -> "rotate"
  | Xrot -> "xrot"
  | Yrot -> "yrot"
  | Zrot -> "zrot"
  | Quaternion -> "quaternion"
  | AxisRotate -> "axis_rotate"
  | Mirror -> "mirror"
  | Affine -> "affine"

(* underscored translation and scaling params since they may be ignored by unit *)
let transform_to_rev_params is_unit transform =
  let about = if is_unit then [] else [ Optional "about", "_p" ] in
  match transform with
  | Translate | Xtrans | Ytrans | Ztrans -> [ Nolabel, "_p" ]
  | Scale | Xscale | Yscale | Zscale -> [ Nolabel, "_s" ]
  | Rotate | Xrot | Yrot | Zrot -> (Nolabel, "r") :: about
  | Quaternion -> (Nolabel, "q") :: about
  | AxisRotate -> (Nolabel, "a") :: (Nolabel, "ax") :: about
  | Mirror -> [ Nolabel, "ax" ]
  | Affine -> [ Nolabel, "m" ]

let transform_to_names is_unit transform =
  match is_unit, transform with
  | true, (Translate | Xtrans | Ytrans | Ztrans | Scale) -> None
  | u, trans -> Some (transform_to_string trans, transform_to_rev_params u trans)

let transform_expr ~loc ~jane ~transform ~kind (ct : core_type) =
  let inner_expr (attrs : Attr.t) lid =
    ( if not attrs.ignored
    then
      transform_to_names attrs.unit transform
      |> Option.map (fun (name, params) ->
             let params =
               List.fold_left
                 (fun ps (lbl, p) -> (lbl, pexp_ident ~loc { loc; txt = lident p }) :: ps)
                 []
                 params
             and txt = Util.fun_id name lid in
             pexp_apply ~loc (pexp_ident ~loc { loc; txt }) params )
    else None )
    |> Option.value ~default:[%expr fun a -> a]
  and fix_id lib m = Longident.(Ldot (Ldot (lident lib, m), "t")) in
  let expr_of_typ attrs ct =
    let apply_map_exprs (expr, maps) =
      List.fold_left (fun expr m -> [%expr [%e m ~loc expr]]) expr maps
    in
    let rec aux attrs funcs next =
      let attrs = Attr.update ~loc attrs (`Type next) in
      match next with
      | [%type: [%t? typ] option] | [%type: [%t? typ] Option.t] ->
        aux attrs (Util.option_map_expr :: funcs) typ
      | [%type: [%t? typ] list] | [%type: [%t? typ] List.t] ->
        aux attrs (Util.list_map_expr :: funcs) typ
      | [%type: ([%t? typ], [%t? _]) result] | [%type: ([%t? typ], [%t? _]) Result.t] ->
        aux attrs (Util.result_map_expr :: funcs) typ
      | [%type: ([%t? _], [%t? _], [%t? _]) Scad.t]
      | [%type: Scad.d2]
      | [%type: Scad.d3]
      | [%type: ([%t? _], [%t? _], [%t? _]) OSCADml.Scad.t]
      | [%type: OSCADml.Scad.d2]
      | [%type: OSCADml.Scad.d3] -> inner_expr attrs (fix_id "OSCADml" "Scad"), funcs
      | [%type: v2] | [%type: OCADml.v2] -> inner_expr attrs (fix_id "OCADml" "V2"), funcs
      | [%type: v3] | [%type: OCADml.v3] -> inner_expr attrs (fix_id "OCADml" "V3"), funcs
      | { ptyp_desc = Ptyp_tuple cts; _ } ->
        let tup_expr =
          let argn n = Printf.sprintf "arg%i" n in
          let args =
            let arg_var i _ = ppat_var ~loc { loc; txt = argn i } in
            ppat_tuple ~loc (List.mapi arg_var cts)
          and sub_exprs =
            let f i c =
              let expr = apply_map_exprs (aux attrs [] c) in
              [%expr [%e expr] [%e evar ~loc (argn i)]]
            in
            List.mapi f cts
          in
          [%expr fun [%p args] -> [%e pexp_tuple ~loc sub_exprs]]
        in
        tup_expr, funcs
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, []); _ } ->
        inner_expr attrs lid, funcs
      | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, (arg :: _ as args)); _ } ->
        if List.for_all (Fun.negate Util.is_constr) args
        then inner_expr attrs lid, funcs
        else aux attrs (Util.map_expr ~lid ~jane:attrs.jane :: funcs) arg
      | ct -> Location.raise_errorf ~loc "Unhandled type: %s" (string_of_core_type ct)
    in
    apply_map_exprs (aux attrs [] ct)
  in
  let attrs = Attr.(update ~loc { unit = false; ignored = false; jane } kind) in
  if attrs.ignored then [%expr fun a -> a] else expr_of_typ attrs ct

let transformer ~loc ~transform (td : type_declaration) expr =
  let name =
    let func_name = transform_to_string transform in
    ppat_var
      ~loc
      { loc
      ; txt =
          ( if String.equal td.ptype_name.txt "t"
          then func_name
          else Printf.sprintf "%s_%s" func_name td.ptype_name.txt )
      }
  and func =
    let f expr (lbl, txt) = pexp_fun ~loc lbl None (ppat_var ~loc { loc; txt }) expr
    and init = pexp_fun ~loc Nolabel None (ppat_var ~loc { loc; txt = "t" }) expr in
    List.fold_left f init (transform_to_rev_params false transform)
  in
  [%stri let [%p name] = [%e func]]

let abstract_transformer ~loc ~jane ~transform (td : type_declaration) ct =
  let expr = transform_expr ~loc ~jane ~transform ~kind:(`Type ct) ct in
  transformer ~loc ~transform td [%expr [%e expr] t]

let record_transformer ~loc ~jane ~transform (td : type_declaration) fields =
  let entry (ld : label_declaration) =
    let loc = ld.pld_loc in
    let field_id = { loc; txt = lident ld.pld_name.txt } in
    let field_expr =
      pexp_field ~loc (pexp_ident ~loc { loc; txt = lident "t" }) field_id
    in
    let expr = transform_expr ~loc ~jane ~transform ~kind:(`Field ld) ld.pld_type in
    field_id, [%expr [%e expr] [%e field_expr]]
  in
  let expr = pexp_record ~loc (List.map entry fields) None in
  transformer ~loc ~transform td expr

let transformer_impl ~jane ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let transforms = function
    | Dim.D2 | Poly _ -> transforms_2d
    | D3 -> transforms_3d
  in
  let f (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_variant _ | Ptype_open; _ } ->
      Location.raise_errorf
        ~loc
        "Deriving scad transformers for variant/open types is not supported."
    | { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } ->
      Location.raise_errorf
        ~loc
        "Scad transformers cannot be derived for empty abstract types."
    | { ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _ } ->
      List.map
        (fun transform -> abstract_transformer ~loc ~jane ~transform td ct)
        (transforms @@ Dim.decide_type ~loc ct)
    | { ptype_kind = Ptype_record fields; _ } ->
      List.map
        (fun transform -> record_transformer ~loc ~jane ~transform td fields)
        (transforms @@ Dim.decide_record ~loc fields)
  in
  List.concat_map f type_declarations

let cad_type_arrow ?(lbl = Nolabel) ~loc name =
  let txt = Longident.Ldot (Longident.Ldot (lident "OCADml", name), "t") in
  ptyp_arrow ~loc lbl (ptyp_constr ~loc { loc; txt } [])

let float_type_arrow ~loc =
  ptyp_arrow ~loc Nolabel (ptyp_constr ~loc { loc; txt = lident "float" } [])

let var_type_arrow ?(lbl = Nolabel) ~loc v = ptyp_arrow ~loc lbl (ptyp_var ~loc v)

let transformer_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let f ({ ptype_kind; ptype_name; ptype_params; ptype_manifest; _ } as td) =
    let dim =
      match ptype_kind, ptype_manifest with
      | (Ptype_variant _ | Ptype_open), _ ->
        Location.raise_errorf
          ~loc
          "Deriving scad transformers for non-abstract/record types is not supported."
      | Ptype_abstract, Some ct -> Dim.decide_type ~loc ct
      | Ptype_abstract, None ->
        Location.raise_errorf
          ~loc
          "Scad transformers cannot be derived for empty abstract types."
      | Ptype_record fields, _ -> Dim.decide_record ~loc fields
    in
    let space_arrow, rot_arrow, about_arrow, affine_arrow, transforms =
      match dim with
      | D2 ->
        ( cad_type_arrow ~loc "V2"
        , float_type_arrow ~loc
        , cad_type_arrow ~lbl:(Optional "about") ~loc "V2"
        , cad_type_arrow ~loc "Affine2"
        , transforms_2d )
      | D3 ->
        let v3_arrow = cad_type_arrow ~loc "V3" in
        ( v3_arrow
        , v3_arrow
        , cad_type_arrow ~lbl:(Optional "about") ~loc "V3"
        , cad_type_arrow ~loc "Affine3"
        , transforms_3d )
      | Poly (space, rot, affine) ->
        ( var_type_arrow ~loc space
        , var_type_arrow ~loc rot
        , var_type_arrow ~lbl:(Optional "about") ~loc space
        , var_type_arrow ~loc affine
        , transforms_2d )
    in
    let gen_sig transform =
      let name =
        let func_name = transform_to_string transform in
        if String.equal td.ptype_name.txt "t"
        then func_name
        else Printf.sprintf "%s_%s" func_name td.ptype_name.txt
      and last_arrow =
        let typ =
          ptyp_constr
            ~loc
            { loc; txt = lident ptype_name.txt }
            (List.map fst ptype_params)
        in
        ptyp_arrow ~loc Nolabel typ typ
      in
      let pval_type =
        match transform with
        | Rotate -> about_arrow @@ rot_arrow last_arrow
        | Xtrans | Ytrans | Ztrans | Xscale | Yscale | Zscale ->
          float_type_arrow ~loc @@ last_arrow
        | Xrot | Yrot | Zrot -> about_arrow @@ float_type_arrow ~loc @@ last_arrow
        | Affine -> affine_arrow last_arrow
        | Quaternion -> about_arrow @@ cad_type_arrow ~loc "Quaternion" @@ last_arrow
        | AxisRotate -> about_arrow @@ space_arrow @@ float_type_arrow ~loc @@ last_arrow
        | _ -> space_arrow last_arrow
      in
      psig_value
        ~loc
        { pval_name = { loc; txt = name }
        ; pval_type
        ; pval_attributes = []
        ; pval_loc = loc
        ; pval_prim = []
        }
    in
    List.map gen_sig transforms
  in
  List.concat_map f type_declarations

let impl_generator ~jane = Deriving.Generator.V2.make_noarg (transformer_impl ~jane)
let intf_generator = Deriving.Generator.V2.make_noarg transformer_intf

let cad =
  Deriving.add
    ~str_type_decl:(impl_generator ~jane:false)
    ~sig_type_decl:intf_generator
    "cad"

let cad_jane =
  Deriving.add
    ~str_type_decl:(impl_generator ~jane:true)
    ~sig_type_decl:intf_generator
    "cad_jane"
