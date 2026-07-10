open Core

(* A plain C include (headers with their own [extern "C"] guards are fine to put inside
   the generated [extern "C" { ... }] wrapper). *)
[%%c
  {|
#include <math.h>
#include <string.h>
|}]

(* A C++ only include and helper function. These land outside of the [extern "C" { ... }]
   wrapper, which is required for C++ standard library headers to compile. *)
[%%cpp
  {|
#include <string>
#include <vector>

namespace ppx_c_bindings_test_cpp {
  static std::string greet(const std::string& who) {
    return std::string("hello, ") + who;
  }
}
|}]

(* Custom type, wrapped automatically in [extern "C" { ... }]. *)
type fake_str = [%c {|char*|} ~free:{|free(*t);|}]

let make_fake_str str =
  let t = alloc_fake_str () in
  [%c.no_alloc
    {|
      *Fake_str_val(%{t:fake_str value}) =
        (char*)calloc(caml_string_length(%{str:string value})+1,sizeof(char));
      strcpy(*Fake_str_val(%{t}), String_val(%{str}));
      |}];
  t
;;

let get_fake_str t =
  [%c.alloc
    ({| CAMLreturn(caml_copy_string(*Fake_str_val(%{t:fake_str value}))); |}
     : string value)]
;;

(* Exercise the [%%cpp] helper defined above from a binding. The fully-qualified name
   [::ppx_c_bindings_test_cpp::greet] is used defensively for clarity; [extern "C"] only
   affects linkage, so unqualified or partially-qualified name lookup would also work
   here. *)
let greet name =
  [%c.alloc
    ({|
       std::string s = ::ppx_c_bindings_test_cpp::greet(String_val(%{name:string value}));
       CAMLreturn(caml_copy_string(s.c_str()));
     |}
     : string value)]
;;

let%expect_test _ =
  let a = make_fake_str "foo" in
  print_endline (get_fake_str a);
  [%expect {| foo |}]
;;

let%expect_test _ =
  print_endline (greet "world");
  [%expect {| hello, world |}]
;;

let pi = [%c.no_alloc ({|return M_PI;|} : float)]

let%expect_test _ =
  printf "%g" pi;
  [%expect {| 3.14159 |}]
;;
