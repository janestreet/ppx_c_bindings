open! Core
open! Ppxlib
open! Import

module Config = struct
  type t = { line_numbers : bool }

  let param =
    let%map_open.Command () = return ()
    and line_numbers =
      flag "no-line-numbers" no_arg ~doc:" Do not include line numbers in the output"
      >>| not
    in
    { line_numbers }
  ;;
end

module Defs : sig
  type t

  val empty : t
  val update_path : t -> path:string list -> t

  val define
    :  t
    -> unique_name:string
    -> string
    -> print_master_define:(name:string -> unit)
    -> t * string
end = struct
  type t =
    { outer : t option
    ; name : string
    ; defines : string String.Map.t
    }

  let empty = { outer = None; name = "ROOT"; defines = String.Map.empty }

  let rec path t =
    match t.outer with
    | None -> []
    | Some outer -> path outer @ [ t.name ]
  ;;

  let rec get_defined t name =
    match Map.find t.defines name with
    | Some name -> Some name
    | None ->
      (match t.outer with
       | Some t -> get_defined t name
       | None -> None)
  ;;

  let pop_path t =
    match t.outer with
    | None -> t
    | Some outer ->
      Map.fold t.defines ~init:outer ~f:(fun ~key ~data outer ->
        print_endline [%string "#undef %{key}"];
        Option.iter (get_defined outer key) ~f:(fun data ->
          print_endline [%string "#define %{key} %{data}"]);
        let key = [%string "%{t.name}__%{key}"] in
        Option.iter (get_defined outer key) ~f:(fun _data ->
          print_endline [%string "#undef %{key}"]);
        print_endline [%string "#define %{key} %{data}"];
        { outer with defines = Map.set outer.defines ~key ~data })
  ;;

  let push_path t ~name = { outer = Some t; name; defines = String.Map.empty }

  let rec strip_common_prefix a b =
    match a, b with
    | [], _ | _, [] -> a, b
    | a_hd :: a_tl, b_hd :: b_tl ->
      if String.equal a_hd b_hd then strip_common_prefix a_tl b_tl else a, b
  ;;

  let update_path t ~path:target_path =
    let to_pop, to_push = strip_common_prefix (path t) target_path in
    let t =
      List.fold (List.rev to_pop) ~init:t ~f:(fun t name ->
        assert (String.equal name t.name);
        pop_path t)
    in
    let t = List.fold to_push ~init:t ~f:(fun t name -> push_path t ~name) in
    t
  ;;

  let define t ~unique_name name ~print_master_define =
    let unique_name =
      let path = path t |> String.concat ~sep:"__" in
      [%string "__PPX_C_BINDINGS__DEF__IMPL__%{path}__%{name}__%{unique_name}__"]
    in
    print_master_define ~name:unique_name;
    Option.iter (get_defined t name) ~f:(fun _data ->
      print_endline [%string "#undef %{name}"]);
    print_endline [%string "#define %{name} %{unique_name}"];
    { t with defines = Map.set t.defines ~key:name ~data:unique_name }, unique_name
  ;;
end

let print' ~(config : Config.t) here code =
  if config.line_numbers
  then print_endline [%string "#line %{here.pos_lnum#Int} \"%{here.pos_fname}\""];
  print_endline code;
  ()
;;

let print ~config { loc = { loc_start = here; _ }; txt = code } = print' ~config here code
let top_level ~config ~loc code = print' ~config loc.loc_start code

let type_def ~config ~defs ~loc t =
  let unique_name = C_type_def.unique_name ~loc t in
  let defs, internal_unpack_f =
    Defs.define
      defs
      ~unique_name
      [%string "%{(C_type_def.name t).txt |> String.capitalize}_val"]
      ~print_master_define:(fun ~name ->
        print'
          ~config
          (C_type_def.name t).loc.loc_start
          [%string
            {|
static inline %{(C_type_def.c_type t).txt}* %{name}(value X) {
  return (%{(C_type_def.c_type t).txt}*)Data_custom_val(X);
}|}])
  in
  let custom_operations =
    let finalizer_op =
      match C_type_def.free t with
      | None -> "custom_finalize_default"
      | Some code ->
        let op_name = [%string "__caml_ops__free_%{unique_name}"] in
        print'
          ~config
          [%here]
          [%string
            {|
static void %{op_name}(value __t_val) {
  %{(C_type_def.c_type t).txt} *t = %{internal_unpack_f}(__t_val);
|}];
        print ~config code;
        print'
          ~config
          [%here]
          [%string
            {|
  return;
}
|}];
        op_name
    in
    let op_name = [%string "__caml_ops__%{unique_name}"] in
    print'
      ~config
      [%here]
      [%string
        {|
static struct custom_operations %{op_name} = {
      "%{unique_name}",
      %{finalizer_op},
      custom_compare_default,
      custom_hash_default,
      custom_serialize_default,
      custom_deserialize_default,
      custom_compare_default,
      custom_fixed_length_default,
  };
|}];
    op_name
  in
  print'
    ~config
    [%here]
    [%string
      {|
CAMLprim value alloc_%{unique_name}() {
      CAMLparam0();
      CAMLlocal1(__t_val);
      __t_val=caml_alloc_custom(&%{custom_operations},sizeof(%{(C_type_def.c_type t).txt}),0,1);
      CAMLreturn(__t_val);
}
|}];
  defs
;;

let var_name name = [%string "__ppx_c_bindings__arg__%{name}"]
let var_to_string ?type_:_ name = [%string "(%{var_name name})"]

let print_code ~config (code : C_expression.t) =
  print
    ~config
    (Loc.map (C_expression.code code) ~f:(C_template.to_string ~var_to_string))
;;

let caml_params ~config gc_root_args =
  if List.is_empty gc_root_args
  then print' ~config [%here] [%string "CAMLparam0();"]
  else
    gc_root_args
    |> List.chunks_of ~length:5
    |> List.iteri ~f:(fun i args ->
      let ext = if i > 0 then "x" else "" in
      let nargs = List.length args in
      let args = args |> String.concat ~sep:", " in
      print' ~config [%here] [%string "CAML%{ext}param%{nargs#Int}(%{args});"])
;;

let native_function ~config ~loc ~native_function_name (code : C_expression.t) =
  let return_type =
    match C_expression.return code with
    | None -> "value"
    | Some type_ -> Type_.to_c_type type_
  in
  let fn_args =
    C_expression.args code
    |> Map.to_alist
    |> List.map ~f:(fun (name, type_) ->
      [%string "%{Type_.to_c_type type_} %{var_name name}"])
    |> String.concat ~sep:", "
  in
  print'
    ~config
    loc.loc_start
    [%string "CAMLprim %{return_type} %{native_function_name}(%{fn_args}) {"];
  if not (C_expression.alloc code)
  then ()
  else (
    let gc_root_args =
      C_expression.args code
      |> Map.to_alist
      |> List.filter_map ~f:(fun (name, type_) ->
        match type_ with
        | Value _ | Local_value _ -> Some (var_name name)
        | Int | Int32 | Int64 | Float -> None)
    in
    caml_params ~config gc_root_args);
  print_code ~config code;
  if Option.is_none (C_expression.return code)
  then
    if C_expression.alloc code
    then print' ~config [%here] "CAMLreturn(Val_unit);"
    else print' ~config [%here] "return Val_unit;";
  print' ~config [%here] "}"
;;

let bytecode_wrapper_function
  ~config
  ~loc
  ~native_function_name
  ~bytecode_function_name
  (code : C_expression.t)
  =
  let unpack expr ~type_ =
    match (type_ : Type_.t) with
    | Value _ | Local_value _ -> expr
    | Int -> [%string "Int_val(%{expr})"]
    | Int32 -> [%string "Int32_val(%{expr})"]
    | Int64 -> [%string "Int64_val(%{expr})"]
    | Float -> [%string "Double_val(%{expr})"]
  in
  let pack expr ~type_ =
    match (type_ : Type_.t) with
    | Value _ | Local_value _ -> expr
    | Int -> [%string "Val_int(%{expr})"]
    | Int32 -> [%string "caml_copy_int32(%{expr})"]
    | Int64 -> [%string "caml_copy_int64(%{expr})"]
    | Float -> [%string "caml_copy_double(%{expr})"]
  in
  let args = C_expression.args code |> Map.to_alist in
  let fn_args, ignore_extra_args, call_args =
    if List.length args > 5
    then
      ( "value* argv, int argn"
      , "((void)((argn)));"
      , args
        |> List.mapi ~f:(fun i (name, type_) ->
          unpack ~type_ [%string "argv[%{i#Int}] /* %{name} */"])
        |> String.concat ~sep:", " )
    else
      ( args
        |> List.map ~f:(fun (name, _) -> [%string "value %{name}"])
        |> String.concat ~sep:", "
      , ""
      , args
        |> List.map ~f:(fun (name, type_) -> unpack name ~type_)
        |> String.concat ~sep:", " )
  in
  print'
    ~config
    loc.loc_start
    [%string "CAMLprim value %{bytecode_function_name}(%{fn_args}) {"];
  (match C_expression.return code with
   | None ->
     print'
       ~config
       [%here]
       [%string
         "%{ignore_extra_args}\n%{native_function_name}(%{call_args});\nreturn Val_unit;"]
   | Some type_ ->
     let expr = [%string "%{native_function_name}(%{call_args})"] in
     if Type_.pack_allocates type_ then caml_params ~config (List.map args ~f:fst);
     print' ~config [%here] [%string.global "%{ignore_extra_args}"];
     if Type_.pack_allocates type_
     then print' ~config [%here] [%string "CAMLreturn(%{pack ~type_ expr});"]
     else print' ~config [%here] [%string "return %{pack ~type_ expr};"]);
  print' ~config [%here] "}"
;;

let expression ~config ~loc code =
  let unique_name = C_expression.unique_name ~loc code in
  if C_expression.native_and_bytecode_differ code
  then (
    let native_function_name = unique_name ^ "_native" in
    let bytecode_function_name = unique_name ^ "_bytecode" in
    native_function ~config ~loc ~native_function_name code;
    bytecode_wrapper_function
      ~config
      ~loc
      ~native_function_name
      ~bytecode_function_name
      code)
  else native_function ~config ~loc ~native_function_name:unique_name code
;;

let command () =
  Command.basic
    ~summary:"Generate the supporting C code for [ppx_c_binding]"
    (let%map_open.Command file = anon ("FILE" %: Filename_unix.arg_type)
     and config = Config.param in
     fun () ->
       print'
         ~config
         [%here]
         {|
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdlib.h>
|};
       In_channel.with_file file ~f:(fun in_channel ->
         let lexer = Lexing.from_channel ~with_positions:true in_channel in
         Lexing.set_filename lexer file;
         Parse.implementation lexer)
       |> C_block.find_all
       |> List.fold ~init:Defs.empty ~f:(fun defs (path, { loc; txt = code }) ->
         let defs = Defs.update_path defs ~path in
         match code with
         | Top_level code ->
           top_level ~config ~loc code;
           defs
         | Type_def code -> type_def ~config ~defs ~loc code
         | Expression code ->
           expression ~config ~loc code;
           defs)
       |> Defs.update_path ~path:[]
       |> (ignore : Defs.t -> unit))
;;
