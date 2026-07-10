open! Core
open! Ppxlib
open! Import

let compare_core_type a b =
  String.compare
    (format_string Pprintast.core_type a)
    (format_string Pprintast.core_type b)
;;

type t =
  | Int
  | Int32
  | Int64
  | Float
  | Value of core_type
  | Local_value of core_type
  | U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | F32
  | F64
  | Isize
  | Mem
  | Ptr_ext of core_type
  | Ptr_ext_imm of core_type
  | Addr_ext of core_type
  | Addr_ext_imm of core_type
  | Unboxed_tuple of (string option * t) list
[@@deriving compare, sexp_of, variants]

let of_core_type =
  let rec loop type_ ~in_unboxed_tuple =
    let loc = type_.ptyp_loc in
    let not_supported ?(here = Stdlib.Lexing.dummy_pos) reason =
      Location.raise_errorf
        ~loc
        !"ppx_c_bindings: [%s] is not currently supported %s\n\
          If you think the compiler now supports this try removing the assert at:\n\
         \    %{Source_code_position}\n\
          and running ppx_c_bindings tests."
        (format_string Pprintast.core_type type_)
        reason
        here
        ()
    in
    let assert_not_in_unboxed_tuple ?(here = Stdlib.Lexing.dummy_pos) alternatives =
      if in_unboxed_tuple
      then not_supported ~here ("as an unboxed tuple element.\n" ^ alternatives);
      ()
    in
    match type_ with
    | [%type: int] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.i64], [int value] or similar instead.";
      Int
    | [%type: Int32.t] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.i32], [Int32.t value] or similar instead.";
      Int32
    | [%type: Int64.t] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.i64], [Int64.t value] or similar instead.";
      Int64
    | [%type: float] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.f64], [float value] or similar instead.";
      Float
    | [%type: [%t? typ] value] -> Value typ
    | [%type: [%t? typ] local_value] -> Local_value typ
    | [%type: Ox.u8] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.u64], [int value] or similar instead.";
      U8
    | [%type: Ox.i8] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.i64], [int value] or similar instead.";
      I8
    | [%type: Ox.u16] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.u64], [int value] or similar instead.";
      U16
    | [%type: Ox.i16] ->
      assert_not_in_unboxed_tuple
        "[Ox.i16] is not currently supported as an unboxed tuple element.\n\
         Consider using [Ox.i64], [int value] or similar instead.";
      I16
    | [%type: Ox.u32] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.u64], [Int32.t value] or similar instead.";
      U32
    | [%type: Ox.i32] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.i64], [Int32.t value] or similar instead.";
      I32
    | [%type: Ox.u64] -> U64
    | [%type: Ox.i64] -> I64
    | [%type: Ox.f32] ->
      assert_not_in_unboxed_tuple
        "Consider using [Ox.f64], [float value] or similar instead.";
      F32
    | [%type: Ox.f64] -> F64
    | [%type: Ox.isize] -> Isize
    | [%type: Ox.mem] -> Mem
    | [%type: [%t? typ] Ox.Ptr.Ext.t] -> Ptr_ext typ
    | [%type: [%t? typ] Ox.Ptr.Ext.Imm.t] ->
      assert_not_in_unboxed_tuple "Consider using [Ox.Ptr.Ext.t] or similar instead.";
      Ptr_ext_imm typ
    | [%type: [%t? typ] Ox.Addr.Ext.t] -> Addr_ext typ
    | [%type: [%t? typ] Ox.Addr.Ext.Imm.t] ->
      assert_not_in_unboxed_tuple "Consider using [Ox.Addr.Ext.t] or similar instead.";
      Addr_ext_imm typ
    | type_ ->
      (match Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_.ptyp_desc with
       | Ptyp_unboxed_tuple components ->
         (* If you relax any of these please extend the tests in
            [ppx/ppx_c_bindings/test-oxcaml/tuples.ml]. *)
         assert_not_in_unboxed_tuple "Consider in-lining the tuple elements instead.";
         if List.length components <> 2
         then not_supported "only unboxed tuples of size 2 are currently supported.";
         let components =
           List.map components ~f:(fun (label, ty) ->
             label, loop ~in_unboxed_tuple:true ty)
         in
         Unboxed_tuple components
       | _ ->
         Location.raise_errorf
           ~loc
           "ppx_c_bindings: this type construct is not currently supported [%s]"
           (format_string Pprintast.core_type type_)
           ())
  in
  loop ~in_unboxed_tuple:false
;;

module Locality = struct
  type t =
    [ `Global
    | `Local
    ]
  [@@deriving sexp_of, compare, equal]

  let combine ~loc a b =
    if equal a b
    then a
    else Location.raise_errorf ~loc "ppx_c_bindings: Can not mix locality annotations"
  ;;

  let to_mode_annotation ~loc = function
    | `Global -> []
    | `Local -> Ppxlib_jane.Shim.Modes.local ~loc
  ;;
end

let rec locality ~loc : t -> Locality.t option = function
  | Int | Int32 | Int64 | Float -> None
  | Value _ -> Some `Global
  | Local_value _ -> Some `Local
  | U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | F32
  | F64
  | Isize
  | Mem
  | Ptr_ext _
  | Ptr_ext_imm _
  | Addr_ext _
  | Addr_ext_imm _ -> None
  | Unboxed_tuple ts ->
    ts
    |> List.map ~f:(fun (_label, t) -> locality ~loc t)
    |> List.fold ~init:None ~f:(Option.merge ~f:(Locality.combine ~loc))
;;

let attributes ~loc = function
  | Value _ | Local_value _ | Unboxed_tuple _ -> []
  | Int ->
    [ Ppxlib.Ast_builder.Default.attribute
        ~loc
        ~name:{ loc; txt = "untagged" }
        ~payload:(PStr [])
    ]
  | Int32 | Int64 | Float ->
    (* necessary to get the more efficient unboxed calling convention. *)
    [ Ppxlib.Ast_builder.Default.attribute
        ~loc
        ~name:{ loc; txt = "unboxed" }
        ~payload:(PStr [])
    ]
  | U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | F32
  | F64
  | Isize
  | Mem
  | Ptr_ext _
  | Ptr_ext_imm _
  | Addr_ext _
  | Addr_ext_imm _ ->
    (* necessary because the compiler complains otherwise *)
    [ Ppxlib.Ast_builder.Default.attribute
        ~loc
        ~name:{ loc; txt = "unboxed" }
        ~payload:(PStr [])
    ]
;;

let rec to_ocaml_type_without_attributes ~loc t : core_type =
  let loc = { loc with loc_ghost = true } in
  match t with
  | Int -> [%type: int]
  | Int32 -> [%type: Int32.t]
  | Int64 -> [%type: Int64.t]
  | Float -> [%type: float]
  | Value t | Local_value t -> t
  | U8 -> [%type: Ox.u8]
  | I8 -> [%type: Ox.i8]
  | U16 -> [%type: Ox.u16]
  | I16 -> [%type: Ox.i16]
  | U32 -> [%type: Ox.u32]
  | I32 -> [%type: Ox.i32]
  | U64 -> [%type: Ox.u64]
  | I64 -> [%type: Ox.i64]
  | F32 -> [%type: Ox.f32]
  | F64 -> [%type: Ox.f64]
  | Isize -> [%type: Ox.isize]
  | Mem -> [%type: Ox.mem]
  | Ptr_ext t -> [%type: [%t t] Ox.Ptr.Ext.t]
  | Ptr_ext_imm t -> [%type: [%t t] Ox.Ptr.Ext.Imm.t]
  | Addr_ext t -> [%type: [%t t] Ox.Addr.Ext.t]
  | Addr_ext_imm t -> [%type: [%t t] Ox.Addr.Ext.Imm.t]
  | Unboxed_tuple ts ->
    Ppxlib_jane.Ast_builder.Default.ptyp_unboxed_tuple
      ~loc
      (List.map ts ~f:(fun (label, t) -> label, to_ocaml_type_without_attributes ~loc t))
;;

let to_ocaml_type ~loc t ~default_locality =
  let type_ = to_ocaml_type_without_attributes ~loc t in
  let type_ =
    { type_ with ptyp_attributes = type_.ptyp_attributes @ attributes ~loc t }
  in
  let locality =
    Locality.to_mode_annotation
      ~loc
      (locality ~loc t |> Option.value ~default:default_locality)
  in
  type_, locality
;;

let rec to_c_type t =
  match t with
  | Int -> `Primitive "intnat"
  | Int32 -> `Primitive "int32_t"
  | Int64 -> `Primitive "int64_t"
  | Float -> `Primitive "double"
  | Value _ -> `Primitive "value"
  | Local_value _ -> `Primitive "value"
  | U8 -> `Primitive "uint8_t"
  | I8 -> `Primitive "int8_t"
  | U16 -> `Primitive "uint16_t"
  | I16 -> `Primitive "int16_t"
  | U32 -> `Primitive "uint32_t"
  | I32 -> `Primitive "int32_t"
  | U64 -> `Primitive "uint64_t"
  | I64 -> `Primitive "int64_t"
  | F32 -> `Primitive "float"
  | F64 -> `Primitive "double"
  | Isize -> `Primitive "ssize_t"
  | Mem -> `Primitive "void*"
  | Ptr_ext _ -> `Primitive "void*"
  | Ptr_ext_imm _ -> `Primitive "const void*"
  | Addr_ext _ -> `Primitive "void*"
  | Addr_ext_imm _ -> `Primitive "const void*"
  | Unboxed_tuple fields ->
    let fields =
      List.mapi fields ~f:(fun i (name, t) ->
        let name =
          Option.value_or_thunk name ~default:(fun () -> sprintf "_%d" (i + 1))
        in
        let c_type =
          match to_c_type t with
          | `Primitive type_ -> type_
          | `Struct struct_ -> [%string {| struct %{struct_} |}]
        in
        [%string {| %{c_type} %{name}; |}])
      |> String.concat ~sep:" "
    in
    `Struct [%string {| { %{fields} } |}]
;;

let of_string str = parse_string Ppxlib.Parse.core_type ("(" ^ str ^ ")") |> of_core_type

let%expect_test "of_string handles white space and parens" =
  let test str =
    match of_string str with
    | exception _ -> print_endline "failed to parse input"
    | t ->
      print_s [%sexp (t : t)];
      ()
  in
  test "int";
  [%expect {| Int |}];
  test "float";
  [%expect {| Float |}];
  test "   Int64.t ";
  [%expect {| Int64 |}];
  test "   Int32   .  t ";
  [%expect {| Int32 |}];
  test "   ( ( (string) value ))";
  [%expect {| (Value string) |}];
  test "foo value";
  [%expect {| (Value foo) |}];
  test "(a,b) thing Map.M(String).t value";
  [%expect {| (Value "(a, b) thing Map.M(String).t") |}]
;;

let is_gc_root ~loc = function
  | Value _ | Local_value _ -> true
  | Int
  | Int32
  | Int64
  | Float
  | U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | F32
  | F64
  | Isize
  | Mem
  | Ptr_ext _
  | Ptr_ext_imm _
  | Addr_ext _
  | Addr_ext_imm _ -> false
  | Unboxed_tuple _ ->
    Location.raise_errorf
      ~loc
      "ppx_c_bindings:bug: we don't currently allow unboxed tuples in argument position.\n\
       If we do we will need to correctly register any roots that are in the tuple."
;;

module For_testing = struct
  let ocaml_to_c ocaml =
    match of_string ocaml |> to_c_type with
    | `Primitive c -> c
    | `Struct struct_ ->
      raise_s
        [%message
          "not implemented: c type for tuples requires an additional struct def"
            ~ocaml
            ~c:[%string {| struct %{struct_} |}]]
  ;;

  let is_supported_type ocaml =
    match of_string ocaml with
    | _ -> true
    | exception Location.Error _ -> false
  ;;

  let iter = Variants.iter
end
