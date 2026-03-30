open Core

[%%c
  {|
#include <math.h>
#include <string.h>
|}]

type fake_str = [%c {|char*|} ~free:{|free(*t);|}]

let make_fake_str str =
  let t = alloc_fake_str () in
  [%c.no_alloc
    {|
      *Fake_str_val(%{t:fake_str value}) = calloc(caml_string_length(%{str:string value})+1,sizeof(char));
      strcpy(*Fake_str_val(%{t}), String_val(%{str}));
      |}];
  t
;;

let get_fake_str t =
  [%c.alloc
    ({| CAMLreturn(caml_copy_string(*Fake_str_val(%{t:fake_str value}))); |}
     : string value)]
;;

let%expect_test _ =
  let a = make_fake_str "foo" in
  print_endline (get_fake_str a);
  [%expect {| foo |}]
;;

let pi = [%c.no_alloc ({|return M_PI;|} : float)]
let sin a = [%c.alloc ({|CAMLreturn(sin(%{a:float}));|} : float)]
let cos a = [%c.no_alloc ({|return cos(%{a:float});|} : float)]

let%expect_test _ =
  printf "%g" pi;
  [%expect {| 3.14159 |}];
  printf "%g" (sin pi);
  [%expect {| 0 |}];
  printf "%g" (cos pi);
  [%expect {| -1 |}]
;;

let%expect_test "mode crossing" =
  let a = { contended = make_fake_str "foo" } in
  (* Requires [fake_str] to cross contended *)
  print_endline (get_fake_str a.contended);
  [%expect {| foo |}]
;;

let%expect_test "many args" =
  let a = 1 in
  let b = 2 in
  let c = Int32.of_int_exn 3 in
  let d = Int64.of_int_exn 4 in
  let e = 5 in
  let f = 6 in
  printf
    "%d"
    [%c.no_alloc
      ({|return (%{a:int}+%{b:int}+%{c:Int32.t}+%{d:Int64.t}+%{e:int}+%{f:int});|} : int)];
  [%expect {| 21 |}]
;;

let%expect_test "Large ints (32)" =
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x3fabcdef; |} : int)];
  [%expect
    {|
    0x3fabcdef
    0x3fabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x7fabcdef; |} : int)];
  [%expect
    {|
    0x7fabcdef
    0x7fabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0xffabcdef; |} : int)];
  [%expect
    {|
    0xffabcdef
    0xffabcdef
    |}]
;;

let%expect_test "Large ints (64)" =
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x3fabcdef; |} : int)];
  [%expect
    {|
    0x3fabcdef
    0x3fabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x7fabcdef; |} : int)];
  [%expect
    {|
    0x7fabcdef
    0x7fabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0xffabcdef; |} : int)];
  [%expect
    {|
    0xffabcdef
    0xffabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x1ffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x1ffabcdef; |} : int)];
  [%expect
    {|
    0x1ffabcdef
    0x1ffabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdefffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x3fabcdefffabcdef; |} : int)];
  [%expect
    {|
    0x3fabcdefffabcdef
    0x3fabcdefffabcdef
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdefffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0x7fabcdefffabcdef; |} : int)];
  [%expect
    {|
    -0x54321000543211
    -0x54321000543211
    |}];
  printf !"%{Int.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdefffabcdef); |} : int)];
  printf !"%{Int.Hex}\n" [%c.no_alloc ({| return 0xffabcdefffabcdef; |} : int)];
  [%expect
    {|
    -0x54321000543211
    -0x54321000543211
    |}]
;;

let%expect_test ("Large int (32bit)" [@tags "32-bits-only"]) =
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0x3fabcdef; |} : Int32.t)];
  [%expect {| 0x3fabcdef |}];
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0x7fabcdef; |} : Int32.t)];
  [%expect {| 0x7fabcdef |}];
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0xffabcdef; |} : Int32.t)];
  [%expect {| -0x543211 |}]
;;

let%expect_test ("Large ints (64bit)" [@tags "64-bits-only"]) =
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0x3fabcdef; |} : Int64.t)];
  [%expect
    {|
    0x3fabcdef
    0x3fabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0x7fabcdef; |} : Int64.t)];
  [%expect
    {|
    0x7fabcdef
    0x7fabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0xffabcdef; |} : Int64.t)];
  [%expect
    {|
    0xffabcdef
    0xffabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0x1ffabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0x1ffabcdef; |} : Int64.t)];
  [%expect
    {|
    0x1ffabcdef
    0x1ffabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdefffabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0x3fabcdefffabcdef; |} : Int64.t)];
  [%expect
    {|
    0x3fabcdefffabcdef
    0x3fabcdefffabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdefffabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0x7fabcdefffabcdef; |} : Int64.t)];
  [%expect
    {|
    0x7fabcdefffabcdef
    0x7fabcdefffabcdef
    |}];
  printf !"%{Int64.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdefffabcdef); |} : Int64.t)];
  printf !"%{Int64.Hex}\n" [%c.no_alloc ({| return 0xffabcdefffabcdef; |} : Int64.t)];
  [%expect
    {|
    -0x54321000543211
    -0x54321000543211
    |}]
;;

[%%c
  {|
#include <string.h>
#define TEST_HELPER2(N) #N
#define TEST_HELPER1(N) \
       "" #N ": " TEST_HELPER2(N) "\n"
#define TEST_HELPER \
    CAMLreturn(caml_copy_string( \
       TEST_HELPER1(T_val) \
       TEST_HELPER1(Module_a__T_val) \
       TEST_HELPER1(Module_b__T_val) \
       TEST_HELPER1(Submodule__T_val) \
       TEST_HELPER1(Module_b__Submodule__T_val) \
    ));
|}]

let macro_test str =
  String.split_lines str
  |> List.iter ~f:(fun line ->
    let left, right = String.lsplit2_exn line ~on:':' in
    let left = String.strip left in
    let right = String.strip right in
    if String.equal left right
    then print_endline [%string "%{left}: undefined"]
    else print_endline [%string "%{left}: defined"])
;;

let%expect_test _ =
  (* this needs to be duplicated, otherwise it doesn't work *)
  macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
  [%expect
    {|
    T_val: undefined
    Module_a__T_val: undefined
    Module_b__T_val: undefined
    Submodule__T_val: undefined
    Module_b__Submodule__T_val: undefined
    |}]
;;

module Module_a = struct
  type t = [%c {|char|}]

  let%expect_test _ =
    (* this needs to be duplicated, otherwise it doesn't work *)
    macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
    [%expect
      {|
      T_val: defined
      Module_a__T_val: undefined
      Module_b__T_val: undefined
      Submodule__T_val: undefined
      Module_b__Submodule__T_val: undefined
      |}]
  ;;
end

let%expect_test _ =
  (* this needs to be duplicated, otherwise it doesn't work *)
  macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
  [%expect
    {|
    T_val: undefined
    Module_a__T_val: defined
    Module_b__T_val: undefined
    Submodule__T_val: undefined
    Module_b__Submodule__T_val: undefined
    |}]
;;

module Module_b = struct
  let%expect_test _ =
    (* this needs to be duplicated, otherwise it doesn't work *)
    macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
    [%expect
      {|
      T_val: undefined
      Module_a__T_val: defined
      Module_b__T_val: undefined
      Submodule__T_val: undefined
      Module_b__Submodule__T_val: undefined
      |}]
  ;;

  type t = [%c {|char|}]

  let%expect_test _ =
    (* this needs to be duplicated, otherwise it doesn't work *)
    macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
    [%expect
      {|
      T_val: defined
      Module_a__T_val: defined
      Module_b__T_val: undefined
      Submodule__T_val: undefined
      Module_b__Submodule__T_val: undefined
      |}]
  ;;

  let%expect_test _ =
    (* this needs to be duplicated, otherwise it doesn't work *)
    macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
    [%expect
      {|
      T_val: defined
      Module_a__T_val: defined
      Module_b__T_val: undefined
      Submodule__T_val: undefined
      Module_b__Submodule__T_val: undefined
      |}]
  ;;

  module Submodule = struct
    type t = [%c {|char|}]

    let%expect_test _ =
      (* this needs to be duplicated, otherwise it doesn't work *)
      macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
      [%expect
        {|
        T_val: defined
        Module_a__T_val: defined
        Module_b__T_val: undefined
        Submodule__T_val: undefined
        Module_b__Submodule__T_val: undefined
        |}]
    ;;
  end

  let%expect_test _ =
    (* this needs to be duplicated, otherwise it doesn't work *)
    macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
    [%expect
      {|
      T_val: defined
      Module_a__T_val: defined
      Module_b__T_val: undefined
      Submodule__T_val: defined
      Module_b__Submodule__T_val: undefined
      |}]
  ;;
end

let%expect_test _ =
  (* this needs to be duplicated, otherwise it doesn't work *)
  macro_test [%c.alloc ({| TEST_HELPER |} : string value)];
  [%expect
    {|
    T_val: undefined
    Module_a__T_val: defined
    Module_b__T_val: defined
    Submodule__T_val: undefined
    Module_b__Submodule__T_val: defined
    |}]
;;

let (_ : Module_a.t -> _) = Fn.id
let (_ : Module_b.t -> _) = Fn.id
let (_ : Module_b.Submodule.t -> _) = Fn.id
