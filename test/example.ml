open Core

[%%c
  {|
#include <math.h>
#include <string.h>
|}]

type fake_str : immutable_data = [%c {|char*|} ~free:{|free(*t);|}]

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
  printf "%g" Float.pi;
  [%expect {| 3.14159 |}];
  printf "%g" (sin pi);
  [%expect {| 1.22465e-16 |}];
  printf "%g" (Float.sin pi);
  [%expect {| 1.22465e-16 |}];
  printf "%g" (cos pi);
  [%expect {| -1 |}];
  printf "%g" (Float.cos pi);
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

let%expect_test "Large ints (native)" =
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

let%expect_test "Large int (32bit)" =
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0x3fabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0x3fabcdef; |} : Int32.t)];
  [%expect
    {|
    0x3fabcdef
    0x3fabcdef
    |}];
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0x7fabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0x7fabcdef; |} : Int32.t)];
  [%expect
    {|
    0x7fabcdef
    0x7fabcdef
    |}];
  printf !"%{Int32.Hex}\n" [%c.alloc ({| CAMLreturn(0xffabcdef); |} : Int32.t)];
  printf !"%{Int32.Hex}\n" [%c.no_alloc ({| return 0xffabcdef; |} : Int32.t)];
  [%expect
    {|
    -0x543211
    -0x543211
    |}]
;;

let%expect_test "Large ints (64bit)" =
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
       TEST_HELPER1(T_alloc) \
       TEST_HELPER1(Module_a__T_alloc) \
       TEST_HELPER1(Module_b__T_alloc) \
       TEST_HELPER1(Submodule__T_alloc) \
       TEST_HELPER1(Module_b__Submodule__T_alloc) \
       TEST_HELPER1(T_alloc__stack) \
       TEST_HELPER1(Module_a__T_alloc__stack) \
       TEST_HELPER1(Module_b__T_alloc__stack) \
       TEST_HELPER1(Submodule__T_alloc__stack) \
       TEST_HELPER1(Module_b__Submodule__T_alloc__stack) \
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
    T_alloc: undefined
    Module_a__T_alloc: undefined
    Module_b__T_alloc: undefined
    Submodule__T_alloc: undefined
    Module_b__Submodule__T_alloc: undefined
    T_alloc__stack: undefined
    Module_a__T_alloc__stack: undefined
    Module_b__T_alloc__stack: undefined
    Submodule__T_alloc__stack: undefined
    Module_b__Submodule__T_alloc__stack: undefined
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
      T_alloc: defined
      Module_a__T_alloc: undefined
      Module_b__T_alloc: undefined
      Submodule__T_alloc: undefined
      Module_b__Submodule__T_alloc: undefined
      T_alloc__stack: defined
      Module_a__T_alloc__stack: undefined
      Module_b__T_alloc__stack: undefined
      Submodule__T_alloc__stack: undefined
      Module_b__Submodule__T_alloc__stack: undefined
      |}]
  ;;

  (* Exercises the [caml_alloc_custom_local] / [alloc_t__stack] codepath at runtime, so a
     regression in the OCaml-side stub or in the [@noalloc] / [@ local] annotations gets
     caught here and not just by the [example_stubs.c] codegen snapshot.
  *)
  let%expect_test "alloc_t stack/heap" =
    let consume (t @ local) =
      print_s [%message "is_stack_allocated" ~_:(Obj.is_stack (Obj.repr t) : bool)]
    in
    (* check manually constructed name works as epxected *)
    consume (alloc_t__stack ());
    [%expect {| (is_stack_allocated true) |}];
    (* check this works as expected with ppx_emplate *)
    let module _ = struct
      let%template alloc_mode = "stack" [@@alloc stack]
      let%template alloc_mode = "heap" [@@alloc heap]

      [%%template
      [@@@alloc a = (heap, stack)]

      let () =
        print_endline (alloc_mode [@alloc a]);
        consume ((alloc_t [@alloc a]) ())
      ;;]
    end
    in
    [%expect
      {|
      heap
      (is_stack_allocated false)
      stack
      (is_stack_allocated true)
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
    T_alloc: undefined
    Module_a__T_alloc: defined
    Module_b__T_alloc: undefined
    Submodule__T_alloc: undefined
    Module_b__Submodule__T_alloc: undefined
    T_alloc__stack: undefined
    Module_a__T_alloc__stack: defined
    Module_b__T_alloc__stack: undefined
    Submodule__T_alloc__stack: undefined
    Module_b__Submodule__T_alloc__stack: undefined
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
      T_alloc: undefined
      Module_a__T_alloc: defined
      Module_b__T_alloc: undefined
      Submodule__T_alloc: undefined
      Module_b__Submodule__T_alloc: undefined
      T_alloc__stack: undefined
      Module_a__T_alloc__stack: defined
      Module_b__T_alloc__stack: undefined
      Submodule__T_alloc__stack: undefined
      Module_b__Submodule__T_alloc__stack: undefined
      |}]
  ;;

  type t = [%c {|char|} ~free:{|(void)t;|}]

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
      T_alloc: defined
      Module_a__T_alloc: defined
      Module_b__T_alloc: undefined
      Submodule__T_alloc: undefined
      Module_b__Submodule__T_alloc: undefined
      T_alloc__stack: undefined
      Module_a__T_alloc__stack: defined
      Module_b__T_alloc__stack: undefined
      Submodule__T_alloc__stack: undefined
      Module_b__Submodule__T_alloc__stack: undefined
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
      T_alloc: defined
      Module_a__T_alloc: defined
      Module_b__T_alloc: undefined
      Submodule__T_alloc: undefined
      Module_b__Submodule__T_alloc: undefined
      T_alloc__stack: undefined
      Module_a__T_alloc__stack: defined
      Module_b__T_alloc__stack: undefined
      Submodule__T_alloc__stack: undefined
      Module_b__Submodule__T_alloc__stack: undefined
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
        T_alloc: defined
        Module_a__T_alloc: defined
        Module_b__T_alloc: undefined
        Submodule__T_alloc: undefined
        Module_b__Submodule__T_alloc: undefined
        T_alloc__stack: defined
        Module_a__T_alloc__stack: defined
        Module_b__T_alloc__stack: undefined
        Submodule__T_alloc__stack: undefined
        Module_b__Submodule__T_alloc__stack: undefined
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
      T_alloc: defined
      Module_a__T_alloc: defined
      Module_b__T_alloc: undefined
      Submodule__T_alloc: defined
      Module_b__Submodule__T_alloc: undefined
      T_alloc__stack: undefined
      Module_a__T_alloc__stack: defined
      Module_b__T_alloc__stack: undefined
      Submodule__T_alloc__stack: defined
      Module_b__Submodule__T_alloc__stack: undefined
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
    T_alloc: undefined
    Module_a__T_alloc: defined
    Module_b__T_alloc: defined
    Submodule__T_alloc: undefined
    Module_b__Submodule__T_alloc: defined
    T_alloc__stack: undefined
    Module_a__T_alloc__stack: defined
    Module_b__T_alloc__stack: undefined
    Submodule__T_alloc__stack: undefined
    Module_b__Submodule__T_alloc__stack: defined
    |}]
;;

let (_ : Module_a.t -> _) = Fn.id
let (_ : Module_b.t -> _) = Fn.id
let (_ : Module_b.Submodule.t -> _) = Fn.id
