open! Core
open! Ppxlib
open! Import

type t =
  | Int
  | Int32
  | Int64
  | Float
  | Value of string
  | Local_value of string
[@@deriving compare ~localize, sexp_of]

let of_core_type = function
  | [%type: int] -> Int
  | [%type: Int32.t] -> Int32
  | [%type: Int64.t] -> Int64
  | [%type: float] -> Float
  | [%type: [%t? t_] value] -> Value (format_string Pprintast.core_type t_)
  | [%type: [%t? t_] local_value] -> Local_value (format_string Pprintast.core_type t_)
  | type_ ->
    raise_s
      [%message "Unsupported type2" ~_:(format_string Pprintast.core_type type_ : string)]
;;

let to_ocaml_type =
  let loc = Location.none in
  function
  | Int -> [%type: (int[@untagged])], []
  | Int32 -> [%type: (Int32.t[@unboxed])], []
  | Int64 -> [%type: (Int64.t[@unboxed])], []
  | Float -> [%type: (float[@unboxed])], []
  | Value type_ -> parse_string Parse.core_type type_, []
  | Local_value type_ ->
    parse_string Parse.core_type type_, Ppxlib_jane.Shim.Modes.local ~loc
;;

let to_c_type = function
  | Int -> "intnat"
  | Int32 -> "int32_t"
  | Int64 -> "int64_t"
  | Float -> "double"
  | Value _ -> "value"
  | Local_value _ -> "value"
;;

let is_unboxed_or_untagged = function
  | Int | Int32 | Int64 | Float -> true
  | Value _ | Local_value _ -> false
;;

let pack_allocates = function
  | Int | Value _ | Local_value _ -> false
  | Int32 | Int64 | Float -> true
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
