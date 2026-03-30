open! Core
open! Ppxlib
open! Ppxlib_jane.Shim
open! Import

let top_level ~loc ~path:_ _ =
  let loc = { loc with loc_ghost = true } in
  [%stri let () = ()]
;;

let expression ~loc ~path:_ t =
  let loc = { loc with loc_ghost = true } in
  let unique_name = C_expression.unique_name ~loc t in
  let (module B) = Ast_builder.make loc in
  let args = C_expression.args t |> Map.to_alist in
  let fn_args, call_args =
    let open Ppxlib_jane.Ast_builder.Default in
    let result_type, result_modes =
      match C_expression.return t with
      | None -> [%type: unit], []
      | Some return -> Type_.to_ocaml_type return
    in
    let return : Ppxlib_jane.arrow_result = { result_type; result_modes } in
    match args with
    | [] ->
      let arg : Ppxlib_jane.arrow_argument =
        { arg_label = Nolabel
        ; arg_modes = Ppxlib_jane.Shim.Modes.none
        ; arg_type = [%type: unit]
        }
      in
      ptyp_arrow ~loc arg return, [ [%expr ()] ]
    | ts ->
      let build_arrow_argument (arg_type, arg_modes) : Ppxlib_jane.arrow_argument =
        { arg_label = Nolabel; arg_modes; arg_type }
      in
      let final_arrow_result, exprs =
        List.fold_right ts ~init:(return, []) ~f:(fun (name, type_) (return, args) ->
          ( { Ppxlib_jane.result_type =
                ptyp_arrow
                  ~loc
                  (type_ |> Type_.to_ocaml_type |> build_arrow_argument)
                  return
            ; result_modes = Ppxlib_jane.Shim.Modes.none
            }
          , B.evar name :: args ))
      in
      final_arrow_result.result_type, exprs
  in
  [%expr
    let open
      [%m
      B.pmod_structure
        [ B.pstr_primitive
            { (B.value_description
                 ~name:{ txt = "_" ^ unique_name; loc }
                 ~type_:fn_args
                 ~prim:
                   (if C_expression.native_and_bytecode_differ t
                    then [ unique_name ^ "_bytecode"; unique_name ^ "_native" ]
                    else [ unique_name ]))
              with
              pval_attributes =
                (if C_expression.alloc t
                 then []
                 else [ B.attribute ~name:{ loc; txt = "noalloc" } ~payload:(PStr []) ])
            }
        ]] in
    [%e B.eapply (B.evar ("_" ^ unique_name)) call_args]]
;;

let type_def ~loc ~path:_ t =
  let loc = { loc with loc_ghost = true } in
  let unique_name = C_type_def.unique_name ~loc t in
  let (module B) = Ast_builder.make loc in
  [%stri
    include
      [%m
      B.pmod_structure
        [ B.pstr_type
            Recursive
            [ Type_declaration.
                { ptype_name = C_type_def.name t
                ; ptype_params = C_type_def.params t
                ; ptype_cstrs = C_type_def.cstrs t
                ; ptype_kind = Ptype_abstract
                ; ptype_private = Private
                ; ptype_manifest = None
                ; ptype_loc = loc
                ; ptype_jkind_annotation = C_type_def.jkind t
                ; ptype_attributes = []
                }
              |> Type_declaration.to_parsetree
            ]
        ; B.pstr_primitive
            (Value_description.create
               ~name:(Loc.map (C_type_def.name t) ~f:(sprintf "alloc_%s"))
               ~type_:
                 [%type:
                   unit
                   -> [%t
                        B.ptyp_constr
                          (Loc.map (C_type_def.name t) ~f:lident)
                          (List.map (C_type_def.params t) ~f:(Fn.const B.ptyp_any))]]
               ~prim:[ "alloc_" ^ unique_name ]
               ~modalities:[ { txt = Modality "portable"; loc } ]
               ~loc)
        ]]]
;;

let impl = C_block.map_struct ~top_level ~type_def ~expression
