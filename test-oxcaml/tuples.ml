open! Core

(* Returning an unboxed tuple from C:
   - the native function returns a small struct `[{ int64_t _1; int64_t _2; }]` via the C
     ABI small struct convention.

   - user uses normal array/struct init syntax `[{ ... }]`. *)

type 'a value = 'a [@@deriving sexp_of]
type 'a local_value = 'a [@@deriving sexp_of]

module Ox = Ox_with_ptr_sexp

(*$
  open Core
  module Type_ = Ppx_c_bindings_common.For_testing.Type_

  module Type_with_sample_value = struct
    type t =
      { ocaml_type : string
      ; c_value : string
      ; sexp_value : Sexp.t
      }
    [@@deriving sexp_of, fields ~getters]

    let unboxed_tuple ts =
      let t =
        { ocaml_type =
            List.map ts ~f:ocaml_type |> String.concat ~sep:" * " |> sprintf "#( %s )"
        ; c_value = List.map ts ~f:c_value |> String.concat ~sep:", " |> sprintf "{ %s }"
        ; sexp_value = List.map ts ~f:sexp_value |> [%sexp_of: Sexp.t list]
        }
      in
      Option.some_if (Type_.is_supported_type t.ocaml_type) t
    ;;
  end

  let print_test (tuple : Type_with_sample_value.t) =
    print_endline
      [%string
        {ocaml|
        let%expect_test "unboxed tuple return: %{tuple.ocaml_type}" =
          let tuple =
            [%c.no_alloc ({| return %{tuple.c_value}; |} : %{tuple.ocaml_type})]
          in
          print_s
            [%message "" ~_:(tuple : %{tuple.ocaml_type})];
          [%expect {| %{tuple.sexp_value#Sexp} |}]
        ;;
        |ocaml}]
  ;;

  let all_simple_types : Type_with_sample_value.t list =
    let all = ref [] in
    let add ?c_value ocaml_type sexp_value _variant =
      all
      := !all
         @ [ { Type_with_sample_value.ocaml_type
             ; sexp_value = Sexp.of_string sexp_value
             ; c_value = Option.value c_value ~default:sexp_value
             }
           ]
    in
    Ppx_c_bindings_common.For_testing.Type_.iter
      ~int:(add "int" "63001")
      ~int32:(add "Int32.t" "32002")
      ~int64:(add "Int64.t" "64003")
      ~float:(add "float" "64.004")
      ~value:(add "int value" "63005" ~c_value:"Val_int(63005)")
      ~local_value:(fun _ -> (* tested separately. *) ())
      ~u8:(add "Ox.u8" "8007")
      ~i8:(add "Ox.i8" "-8008")
      ~u16:(add "Ox.u16" "16009")
      ~i16:(add "Ox.i16" "-16010")
      ~u32:(add "Ox.u32" "32011")
      ~i32:(add "Ox.i32" "-32012")
      ~u64:(add "Ox.u64" "64013")
      ~i64:(add "Ox.i64" "-64014")
      ~f32:(add "Ox.f32" "32.015")
      ~f64:(add "Ox.f64" "64.016")
      ~isize:(add "Ox.isize" "-64017")
      ~mem:(add "Ox.mem" "0x64018" ~c_value:"( void* )0x64018")
      ~ptr_ext:(add "int Ox.Ptr.Ext.t" "0x64019" ~c_value:"( void* )0x64019")
      ~ptr_ext_imm:(add "int Ox.Ptr.Ext.Imm.t" "0x64020" ~c_value:"( void* )0x64020")
      ~addr_ext:(add "int Ox.Addr.Ext.t" "0x64021" ~c_value:"( void* )0x64021")
      ~addr_ext_imm:(add "int Ox.AddrExt.Imm.t" "0x64022" ~c_value:"( void* )0x64022")
      ~unboxed_tuple:(fun _ -> ());
    !all
  ;;

  let () =
    List.cartesian_product all_simple_types all_simple_types
    |> List.filter_map ~f:(fun (a, b) -> Type_with_sample_value.unboxed_tuple [ a; b ])
    |> List.iter ~f:print_test
  ;;
*)
let%expect_test "unboxed tuple return: #( int value * int value )" =
  let tuple =
    [%c.no_alloc
      ({| return { Val_int(63005), Val_int(63005) }; |} : int value * int value)]
  in
  print_s [%message "" ~_:(tuple : int value * int value)];
  [%expect {| (63005 63005) |}]
;;

let%expect_test "unboxed tuple return: #( int value * Ox.u64 )" =
  let tuple =
    [%c.no_alloc ({| return { Val_int(63005), 64013 }; |} : int value * Ox.u64)]
  in
  print_s [%message "" ~_:(tuple : int value * Ox.u64)];
  [%expect {| (63005 64013) |}]
;;

let%expect_test "unboxed tuple return: #( int value * Ox.i64 )" =
  let tuple =
    [%c.no_alloc ({| return { Val_int(63005), -64014 }; |} : int value * Ox.i64)]
  in
  print_s [%message "" ~_:(tuple : int value * Ox.i64)];
  [%expect {| (63005 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( int value * Ox.f64 )" =
  let tuple =
    [%c.no_alloc ({| return { Val_int(63005), 64.016 }; |} : int value * Ox.f64)]
  in
  print_s [%message "" ~_:(tuple : int value * Ox.f64)];
  [%expect {| (63005 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( int value * Ox.isize )" =
  let tuple =
    [%c.no_alloc ({| return { Val_int(63005), -64017 }; |} : int value * Ox.isize)]
  in
  print_s [%message "" ~_:(tuple : int value * Ox.isize)];
  [%expect {| (63005 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( int value * Ox.mem )" =
  let tuple =
    [%c.no_alloc
      ({| return { Val_int(63005), ( void* )0x64018 }; |} : int value * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : int value * Ox.mem)];
  [%expect {| (63005 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( int value * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { Val_int(63005), ( void* )0x64019 }; |} : int value * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int value * int Ox.Ptr.Ext.t)];
  [%expect {| (63005 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( int value * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { Val_int(63005), ( void* )0x64021 }; |} : int value * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int value * int Ox.Addr.Ext.t)];
  [%expect {| (63005 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * int value )" =
  let tuple =
    [%c.no_alloc ({| return { 64013, Val_int(63005) }; |} : Ox.u64 * int value)]
  in
  print_s [%message "" ~_:(tuple : Ox.u64 * int value)];
  [%expect {| (64013 63005) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * Ox.u64 )" =
  let tuple = [%c.no_alloc ({| return { 64013, 64013 }; |} : Ox.u64 * Ox.u64)] in
  print_s [%message "" ~_:(tuple : Ox.u64 * Ox.u64)];
  [%expect {| (64013 64013) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * Ox.i64 )" =
  let tuple = [%c.no_alloc ({| return { 64013, -64014 }; |} : Ox.u64 * Ox.i64)] in
  print_s [%message "" ~_:(tuple : Ox.u64 * Ox.i64)];
  [%expect {| (64013 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * Ox.f64 )" =
  let tuple = [%c.no_alloc ({| return { 64013, 64.016 }; |} : Ox.u64 * Ox.f64)] in
  print_s [%message "" ~_:(tuple : Ox.u64 * Ox.f64)];
  [%expect {| (64013 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * Ox.isize )" =
  let tuple = [%c.no_alloc ({| return { 64013, -64017 }; |} : Ox.u64 * Ox.isize)] in
  print_s [%message "" ~_:(tuple : Ox.u64 * Ox.isize)];
  [%expect {| (64013 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * Ox.mem )" =
  let tuple =
    [%c.no_alloc ({| return { 64013, ( void* )0x64018 }; |} : Ox.u64 * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : Ox.u64 * Ox.mem)];
  [%expect {| (64013 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc ({| return { 64013, ( void* )0x64019 }; |} : Ox.u64 * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.u64 * int Ox.Ptr.Ext.t)];
  [%expect {| (64013 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.u64 * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc ({| return { 64013, ( void* )0x64021 }; |} : Ox.u64 * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.u64 * int Ox.Addr.Ext.t)];
  [%expect {| (64013 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * int value )" =
  let tuple =
    [%c.no_alloc ({| return { -64014, Val_int(63005) }; |} : Ox.i64 * int value)]
  in
  print_s [%message "" ~_:(tuple : Ox.i64 * int value)];
  [%expect {| (-64014 63005) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * Ox.u64 )" =
  let tuple = [%c.no_alloc ({| return { -64014, 64013 }; |} : Ox.i64 * Ox.u64)] in
  print_s [%message "" ~_:(tuple : Ox.i64 * Ox.u64)];
  [%expect {| (-64014 64013) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * Ox.i64 )" =
  let tuple = [%c.no_alloc ({| return { -64014, -64014 }; |} : Ox.i64 * Ox.i64)] in
  print_s [%message "" ~_:(tuple : Ox.i64 * Ox.i64)];
  [%expect {| (-64014 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * Ox.f64 )" =
  let tuple = [%c.no_alloc ({| return { -64014, 64.016 }; |} : Ox.i64 * Ox.f64)] in
  print_s [%message "" ~_:(tuple : Ox.i64 * Ox.f64)];
  [%expect {| (-64014 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * Ox.isize )" =
  let tuple = [%c.no_alloc ({| return { -64014, -64017 }; |} : Ox.i64 * Ox.isize)] in
  print_s [%message "" ~_:(tuple : Ox.i64 * Ox.isize)];
  [%expect {| (-64014 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * Ox.mem )" =
  let tuple =
    [%c.no_alloc ({| return { -64014, ( void* )0x64018 }; |} : Ox.i64 * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : Ox.i64 * Ox.mem)];
  [%expect {| (-64014 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc ({| return { -64014, ( void* )0x64019 }; |} : Ox.i64 * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.i64 * int Ox.Ptr.Ext.t)];
  [%expect {| (-64014 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.i64 * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { -64014, ( void* )0x64021 }; |} : Ox.i64 * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.i64 * int Ox.Addr.Ext.t)];
  [%expect {| (-64014 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * int value )" =
  let tuple =
    [%c.no_alloc ({| return { 64.016, Val_int(63005) }; |} : Ox.f64 * int value)]
  in
  print_s [%message "" ~_:(tuple : Ox.f64 * int value)];
  [%expect {| (64.016 63005) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * Ox.u64 )" =
  let tuple = [%c.no_alloc ({| return { 64.016, 64013 }; |} : Ox.f64 * Ox.u64)] in
  print_s [%message "" ~_:(tuple : Ox.f64 * Ox.u64)];
  [%expect {| (64.016 64013) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * Ox.i64 )" =
  let tuple = [%c.no_alloc ({| return { 64.016, -64014 }; |} : Ox.f64 * Ox.i64)] in
  print_s [%message "" ~_:(tuple : Ox.f64 * Ox.i64)];
  [%expect {| (64.016 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * Ox.f64 )" =
  let tuple = [%c.no_alloc ({| return { 64.016, 64.016 }; |} : Ox.f64 * Ox.f64)] in
  print_s [%message "" ~_:(tuple : Ox.f64 * Ox.f64)];
  [%expect {| (64.016 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * Ox.isize )" =
  let tuple = [%c.no_alloc ({| return { 64.016, -64017 }; |} : Ox.f64 * Ox.isize)] in
  print_s [%message "" ~_:(tuple : Ox.f64 * Ox.isize)];
  [%expect {| (64.016 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * Ox.mem )" =
  let tuple =
    [%c.no_alloc ({| return { 64.016, ( void* )0x64018 }; |} : Ox.f64 * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : Ox.f64 * Ox.mem)];
  [%expect {| (64.016 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc ({| return { 64.016, ( void* )0x64019 }; |} : Ox.f64 * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.f64 * int Ox.Ptr.Ext.t)];
  [%expect {| (64.016 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.f64 * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { 64.016, ( void* )0x64021 }; |} : Ox.f64 * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.f64 * int Ox.Addr.Ext.t)];
  [%expect {| (64.016 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * int value )" =
  let tuple =
    [%c.no_alloc ({| return { -64017, Val_int(63005) }; |} : Ox.isize * int value)]
  in
  print_s [%message "" ~_:(tuple : Ox.isize * int value)];
  [%expect {| (-64017 63005) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * Ox.u64 )" =
  let tuple = [%c.no_alloc ({| return { -64017, 64013 }; |} : Ox.isize * Ox.u64)] in
  print_s [%message "" ~_:(tuple : Ox.isize * Ox.u64)];
  [%expect {| (-64017 64013) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * Ox.i64 )" =
  let tuple = [%c.no_alloc ({| return { -64017, -64014 }; |} : Ox.isize * Ox.i64)] in
  print_s [%message "" ~_:(tuple : Ox.isize * Ox.i64)];
  [%expect {| (-64017 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * Ox.f64 )" =
  let tuple = [%c.no_alloc ({| return { -64017, 64.016 }; |} : Ox.isize * Ox.f64)] in
  print_s [%message "" ~_:(tuple : Ox.isize * Ox.f64)];
  [%expect {| (-64017 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * Ox.isize )" =
  let tuple = [%c.no_alloc ({| return { -64017, -64017 }; |} : Ox.isize * Ox.isize)] in
  print_s [%message "" ~_:(tuple : Ox.isize * Ox.isize)];
  [%expect {| (-64017 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * Ox.mem )" =
  let tuple =
    [%c.no_alloc ({| return { -64017, ( void* )0x64018 }; |} : Ox.isize * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : Ox.isize * Ox.mem)];
  [%expect {| (-64017 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { -64017, ( void* )0x64019 }; |} : Ox.isize * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.isize * int Ox.Ptr.Ext.t)];
  [%expect {| (-64017 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.isize * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { -64017, ( void* )0x64021 }; |} : Ox.isize * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.isize * int Ox.Addr.Ext.t)];
  [%expect {| (-64017 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * int value )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64018, Val_int(63005) }; |} : Ox.mem * int value)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * int value)];
  [%expect {| (0x64018 63005) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * Ox.u64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64018, 64013 }; |} : Ox.mem * Ox.u64)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * Ox.u64)];
  [%expect {| (0x64018 64013) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * Ox.i64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64018, -64014 }; |} : Ox.mem * Ox.i64)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * Ox.i64)];
  [%expect {| (0x64018 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * Ox.f64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64018, 64.016 }; |} : Ox.mem * Ox.f64)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * Ox.f64)];
  [%expect {| (0x64018 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * Ox.isize )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64018, -64017 }; |} : Ox.mem * Ox.isize)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * Ox.isize)];
  [%expect {| (0x64018 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * Ox.mem )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64018, ( void* )0x64018 }; |} : Ox.mem * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * Ox.mem)];
  [%expect {| (0x64018 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64018, ( void* )0x64019 }; |} : Ox.mem * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * int Ox.Ptr.Ext.t)];
  [%expect {| (0x64018 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( Ox.mem * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64018, ( void* )0x64021 }; |} : Ox.mem * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : Ox.mem * int Ox.Addr.Ext.t)];
  [%expect {| (0x64018 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * int value )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64019, Val_int(63005) }; |} : int Ox.Ptr.Ext.t * int value)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * int value)];
  [%expect {| (0x64019 63005) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * Ox.u64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64019, 64013 }; |} : int Ox.Ptr.Ext.t * Ox.u64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * Ox.u64)];
  [%expect {| (0x64019 64013) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * Ox.i64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64019, -64014 }; |} : int Ox.Ptr.Ext.t * Ox.i64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * Ox.i64)];
  [%expect {| (0x64019 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * Ox.f64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64019, 64.016 }; |} : int Ox.Ptr.Ext.t * Ox.f64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * Ox.f64)];
  [%expect {| (0x64019 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * Ox.isize )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64019, -64017 }; |} : int Ox.Ptr.Ext.t * Ox.isize)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * Ox.isize)];
  [%expect {| (0x64019 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * Ox.mem )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64019, ( void* )0x64018 }; |} : int Ox.Ptr.Ext.t * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * Ox.mem)];
  [%expect {| (0x64019 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64019, ( void* )0x64019 }; |}
       : int Ox.Ptr.Ext.t * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * int Ox.Ptr.Ext.t)];
  [%expect {| (0x64019 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Ptr.Ext.t * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64019, ( void* )0x64021 }; |}
       : int Ox.Ptr.Ext.t * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Ptr.Ext.t * int Ox.Addr.Ext.t)];
  [%expect {| (0x64019 0x64021) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * int value )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, Val_int(63005) }; |} : int Ox.Addr.Ext.t * int value)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * int value)];
  [%expect {| (0x64021 63005) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * Ox.u64 )" =
  let tuple =
    [%c.no_alloc ({| return { ( void* )0x64021, 64013 }; |} : int Ox.Addr.Ext.t * Ox.u64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * Ox.u64)];
  [%expect {| (0x64021 64013) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * Ox.i64 )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, -64014 }; |} : int Ox.Addr.Ext.t * Ox.i64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * Ox.i64)];
  [%expect {| (0x64021 -64014) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * Ox.f64 )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, 64.016 }; |} : int Ox.Addr.Ext.t * Ox.f64)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * Ox.f64)];
  [%expect {| (0x64021 64.016) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * Ox.isize )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, -64017 }; |} : int Ox.Addr.Ext.t * Ox.isize)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * Ox.isize)];
  [%expect {| (0x64021 -64017) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * Ox.mem )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, ( void* )0x64018 }; |} : int Ox.Addr.Ext.t * Ox.mem)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * Ox.mem)];
  [%expect {| (0x64021 0x64018) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * int Ox.Ptr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, ( void* )0x64019 }; |}
       : int Ox.Addr.Ext.t * int Ox.Ptr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * int Ox.Ptr.Ext.t)];
  [%expect {| (0x64021 0x64019) |}]
;;

let%expect_test "unboxed tuple return: #( int Ox.Addr.Ext.t * int Ox.Addr.Ext.t )" =
  let tuple =
    [%c.no_alloc
      ({| return { ( void* )0x64021, ( void* )0x64021 }; |}
       : int Ox.Addr.Ext.t * int Ox.Addr.Ext.t)]
  in
  print_s [%message "" ~_:(tuple : int Ox.Addr.Ext.t * int Ox.Addr.Ext.t)];
  [%expect {| (0x64021 0x64021) |}]
;;

(*$*)

let%expect_test "unboxed local tuples" =
  let a : string = "A" in
  let b : string = "B" in
  let (c, d) : string * string =
    [%c.no_alloc
      ({| return { %{a:string local_value}, %{b:string local_value} }; |}
       : string local_value * string local_value)]
  in
  assert (phys_equal a c);
  assert (phys_equal b d);
  print_endline [%string {| (%{c} %{d}) |}];
  [%expect {| (A B) |}];
  let (e, f) : Ox.i64 * string =
    [%c.no_alloc
      ({| return { 42, %{b:string local_value} }; |} : Ox.i64 * string local_value)]
  in
  assert (phys_equal b f);
  print_endline [%string {| (%{e#Ox.I64} %{f}) |}];
  [%expect {| (42 B) |}];
  let (g, h) : string * Ox.i64 =
    [%c.no_alloc
      ({| return { %{a:string local_value}, 42 }; |} : string local_value * Ox.i64)]
  in
  assert (phys_equal a g);
  print_endline [%string {| (%{g} %{h#Ox.I64}) |}];
  [%expect {| (A 42) |}]
;;
