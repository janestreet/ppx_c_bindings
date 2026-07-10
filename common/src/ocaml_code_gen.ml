open! Core
open! Ppxlib
open! Ppxlib_jane.Shim
open! Import

let top_level ~loc ~path:_ _ =
  let loc = { loc with loc_ghost = true } in
  [%stri let () = ()]
;;

(* [[%%cpp ...]] is a no-op at the OCaml level; the actual C++ content only matters to the
   stub generator. Accepting it unconditionally here means the PPX has no mode flag to
   keep in sync with the stub-generation rule. *)
let cpp_top_level = top_level

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
      | Some return -> Type_.to_ocaml_type ~loc return ~default_locality:`Global
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
                  (type_
                   |> Type_.to_ocaml_type ~default_locality:`Local ~loc
                   |> build_arrow_argument)
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
                 ~prim:[ "PPX_C_BINDINGS_DOES_NOT_SUPPORT_BYTE_CODE"; unique_name ])
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
  let loc_map { txt; loc } ~f =
    (* can't use [Loc.map] since we need to set ghost=true to avoid loc conflicts. *)
    { txt = f txt; loc = { loc with loc_ghost = true } }
  in
  let alloc_external_decl ~heap_or_stack =
    let suffix, return_type_for, extra_attributes =
      let type_ =
        B.ptyp_constr
          (loc_map (C_type_def.name t) ~f:lident)
          (List.map (C_type_def.params t) ~f:(Fn.const B.ptyp_any))
      in
      match (heap_or_stack : Heap_or_stack.t) with
      | Heap -> "", [%type: unit -> [%t type_]], []
      | Stack ->
        ( "__stack"
        , [%type: unit -> [%t type_]]
        , [ B.attribute ~name:{ loc; txt = "noalloc" } ~payload:(PStr []) ] )
    in
    let value_descr =
      Value_description.create
        ~name:
          (loc_map (C_type_def.name t) ~f:(fun name -> {%string|alloc_%{name}%{suffix}|}))
        ~type_:return_type_for
        ~prim:[ {%string|alloc_%{unique_name}%{suffix}|} ]
        ~modalities:[ { txt = Modality "portable"; loc } ]
        ~loc
    in
    B.pstr_primitive
      { value_descr with
        pval_attributes = value_descr.pval_attributes @ extra_attributes
      }
  in
  [%stri
    include
      [%m
      B.pmod_structure
        ([ B.pstr_type
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
         ; alloc_external_decl ~heap_or_stack:Heap
         ]
         @
         if Option.is_some (C_type_def.free t)
         then []
         else [ alloc_external_decl ~heap_or_stack:Stack ])]]
;;

let impl = C_block.map_struct ~top_level ~cpp_top_level ~type_def ~expression
