open! Core
open! Ppxlib
open! Import

module Config = struct
  type t =
    { line_numbers : bool
    ; cpp : bool
    }

  let param =
    let%map_open.Command () = return ()
    and line_numbers =
      flag "no-line-numbers" no_arg ~doc:" Do not include line numbers in the output"
      >>| not
    and cpp =
      flag
        "cpp"
        no_arg
        ~doc:
          " Generate C++ output: wrap each generated binding in an [extern \"C\"] block \
           and emit [%%cpp ...] contents verbatim outside of it (without [-cpp], a \
           [%%cpp ...] block produces a compile-time [#error])"
    in
    { line_numbers; cpp }
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

(* [%%cpp ...] blocks must not be wrapped in [extern "C" { ... }] so that things like
   [#include <iostream>] work correctly. They are emitted verbatim.

   In plain C mode this case is diagnosed by the PPX. If the generator is reached anyway
   (e.g. a user with out-of-sync flags) we emit a [#error] directive instead of the
   snippet; together with the preceding [#line] directive the C compiler reports the error
   at the original [.ml] location with no special formatting needed on our side. *)
let cpp_top_level ~(config : Config.t) ~loc code =
  if config.cpp
  then print' ~config loc.loc_start code
  else
    print'
      ~config
      loc.loc_start
      {|#error "[%%cpp ...] is only supported in C++ mode; pass [-cpp] to [ppx-c-bindings]"|}
;;

(* In C++ mode, wrap an emission in its own [extern "C" { ... }] block so the generated
   symbol has C linkage while still being compiled as C++. Snippets that are emitted as
   C++ (e.g. [[%%cpp ...]]) must NOT be passed through this helper. *)
let with_extern_c ~(config : Config.t) f =
  if config.cpp then print_endline "extern \"C\" {";
  let result = f () in
  if config.cpp then print_endline "}";
  result
;;

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
  let print_internal_alloc defs ~heap_or_stack =
    let alloc_fn_name, suffix =
      match (heap_or_stack : Heap_or_stack.t) with
      | Heap -> "caml_alloc_custom", ""
      | Stack -> "caml_alloc_custom_local", "__stack"
    in
    let defs, internal_alloc_f =
      Defs.define
        defs
        ~unique_name
        [%string "%{(C_type_def.name t).txt |> String.capitalize}_alloc%{suffix}"]
        ~print_master_define:(fun ~name ->
          print'
            ~config
            (C_type_def.name t).loc.loc_start
            [%string
              {|
static inline value %{name}() {
  return %{alloc_fn_name}(&%{custom_operations},sizeof(%{(C_type_def.c_type t).txt}),0,1);
}|}])
    in
    print'
      ~config
      [%here]
      [%string
        {|
CAMLprim value alloc_%{unique_name}%{suffix}() {
      CAMLparam0();
      CAMLlocal1(__t_val);
      __t_val=%{internal_alloc_f}();
      CAMLreturn(__t_val);
}
        |}];
    defs
  in
  let defs = print_internal_alloc defs ~heap_or_stack:Heap in
  let defs =
    if Option.is_some (C_type_def.free t)
    then defs
    else print_internal_alloc defs ~heap_or_stack:Stack
  in
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

let native_function ~config ~loc (code : C_expression.t) =
  let unique_name = C_expression.unique_name ~loc code in
  let fn_args =
    C_expression.args code
    |> Map.to_alist
    |> List.map ~f:(fun (name, type_) ->
      match Type_.to_c_type type_ with
      | `Primitive type_ -> [%string "%{type_} %{var_name name}"]
      | `Struct _ ->
        Location.raise_errorf
          ~loc
          "ppx_c_bindings: we don't currently support unboxed tuples in argument \
           positions"
          ())
    |> String.concat ~sep:", "
  in
  let return_type, additional_boiler_plate =
    match C_expression.return code with
    | None -> "value", Fn.const ()
    | Some type_ ->
      (match Type_.to_c_type type_ with
       | `Primitive "value" -> "value", Fn.const ()
       | `Primitive return_type ->
         (match C_expression.alloc code with
          | false -> return_type, Fn.const ()
          | true ->
            let additional_boiler_plate = function
              | `Before_user_code ->
                (* redefine CAMLreturn to have the correct type instead of value. *)
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                  #undef CAMLreturn
                  #define CAMLreturn(R) CAMLreturnT(%{return_type}, R)
                |}]
              | `Cleanup_at_end ->
                (* revert the change *)
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                    #undef CAMLreturn
                    #define CAMLreturn(R) CAMLreturnT(value, R)
                  |}]
            in
            return_type, additional_boiler_plate)
       | `Struct struct_ ->
         let struct_name = [%string "return_tuple__%{unique_name}"] in
         print' ~config [%here] [%string {| struct %{struct_name} %{struct_}; |}];
         let return_type = [%string {| struct %{struct_name} |}] in
         (match C_expression.alloc code with
          | false ->
            let additional_boiler_plate = function
              | `Before_user_code ->
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                  #ifndef __cplusplus 
                    // C99 requires a cast to use the array/struct initializer syntax
                    #define return return (%{return_type})
                  #else
                    // C++ doesn't allow the cast, but you can prefix with the struct name
                    #define return return %{struct_name} 
                  #endif
                |}]
              | `Cleanup_at_end ->
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                    #undef return
                  |}]
            in
            return_type, additional_boiler_plate
          | true ->
            let additional_boiler_plate = function
              | `Before_user_code ->
                (* redefine CAMLreturn to have the correct type instead of value.

                   We need some additional hacks here to handle the commas inside the
                   macro arguments.
                *)
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                  #undef CAMLreturn
                  #define PPX_C_BINDINGS_VA_ARGS_TUPLE_WRAPPER(...) __VA_ARGS__
                  #ifndef __cplusplus 
                    // C99 requires a cast to use the array/struct initializer syntax
                    #define CAMLreturn(...) CAMLreturnT(%{return_type}, (%{return_type}) PPX_C_BINDINGS_VA_ARGS_TUPLE_WRAPPER ( __VA_ARGS__ ) )
                  #else
                   // C++ doesn't allow the cast, but you can prefix with the struct name
                   #define CAMLreturn(...) CAMLreturnT(%{return_type}, %{struct_name} PPX_C_BINDINGS_VA_ARGS_TUPLE_WRAPPER ( __VA_ARGS__ ) )
                  #endif
                |}]
              | `Cleanup_at_end ->
                (* revert the change *)
                print'
                  ~config
                  [%here]
                  [%string
                    {|
                    #undef PPX_C_BINDINGS_VA_ARGS_TUPLE_WRAPPER
                    #undef CAMLreturn
                    #define CAMLreturn(R) CAMLreturnT(value, R)
                  |}]
            in
            return_type, additional_boiler_plate))
  in
  print'
    ~config
    loc.loc_start
    [%string "CAMLprim %{return_type} %{unique_name}(%{fn_args}) {"];
  if not (C_expression.alloc code)
  then ()
  else (
    let gc_root_args =
      C_expression.args code
      |> Map.to_alist
      |> List.filter_map ~f:(fun (name, type_) ->
        if Type_.is_gc_root ~loc type_ then Some (var_name name) else None)
    in
    caml_params ~config gc_root_args);
  additional_boiler_plate `Before_user_code;
  print_code ~config code;
  if Option.is_none (C_expression.return code)
  then
    if C_expression.alloc code
    then print' ~config [%here] "CAMLreturn(Val_unit);"
    else print' ~config [%here] "return Val_unit;";
  print' ~config [%here] "}";
  additional_boiler_plate `Cleanup_at_end;
  ()
;;

let command () =
  Command.basic
    ~summary:"Generate the supporting C code for [ppx_c_binding]"
    (let%map_open.Command file = anon ("FILE" %: Filename_unix.arg_type)
     and config = Config.param in
     fun () ->
       ();
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
           with_extern_c ~config (fun () -> top_level ~config ~loc code);
           defs
         | Cpp_top_level code ->
           cpp_top_level ~config ~loc code;
           defs
         | Type_def code ->
           with_extern_c ~config (fun () -> type_def ~config ~defs ~loc code)
         | Expression code ->
           with_extern_c ~config (fun () -> native_function ~config ~loc code);
           defs)
       |> Defs.update_path ~path:[]
       |> (ignore : Defs.t -> unit))
;;
