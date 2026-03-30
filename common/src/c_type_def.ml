open! Core
open! Ppxlib
open! Ppxlib_jane.Shim
open! Import

type t =
  { name : string Loc.t
  ; params : (core_type * (variance * injectivity)) list
  ; cstrs : (core_type * core_type * Location.t) list
  ; c_type : string Loc.t
  ; jkind : (jkind_annotation option[@sexp.opaque])
  ; free : string Loc.t option
  }
[@@deriving sexp_of, fields ~getters ~names]

let unique_name = unique_name ~kind:"type" ~sexp_of_t

let of_structure_item (stri : structure_item) =
  match stri.pstr_desc with
  | Ppxlib.Pstr_type
      ( Ppxlib.Recursive
      , [ ({ ptype_name = name
           ; ptype_params = params
           ; ptype_cstrs = cstrs
           ; ptype_kind = _
           ; ptype_loc = _
           ; ptype_private = _
           ; ptype_manifest = Some [%type: [%c [%e? c_type_and_args]]]
           ; ptype_attributes = []
           ; _
           } as type_declaration)
        ] ) ->
    let jkind = Type_declaration.extract_jkind_annotation type_declaration in
    let c_type, args =
      match c_type_and_args.pexp_desc with
      | Ppxlib.Pexp_apply (c_type, args) -> c_type, args
      | _ -> c_type_and_args, []
    in
    let c_type = long_string_constant c_type in
    let args =
      List.fold args ~init:String.Map.empty ~f:(fun args -> function
        | Labelled name, code ->
          assert (List.mem Fields.names name ~equal:String.equal);
          Map.add_exn args ~key:name ~data:(long_string_constant code)
        | Optional _, _ | Ppxlib.Nolabel, _ -> assert false)
    in
    Some { name; params; cstrs; c_type; jkind; free = Map.find args "free" }
  | _ -> None
;;
