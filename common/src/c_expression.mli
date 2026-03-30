open! Core
open! Ppxlib
open! Import

(** Embedded C expressions

    {[
      [%c.alloc {| CODE |}]
    ]}
    {[
      [%c.alloc ({| CODE |}:RET)]
    ]}
    {[
      [%c.no_alloc {| CODE |}]
    ]}
    {[
      [%c.no_alloc ({| CODE |}:RET)]
    ]} *)

type t [@@deriving sexp_of]

val unique_name : loc:Location.t -> t -> string
val args : t -> Type_.t String.Map.t
val return : t -> Type_.t option
val code : t -> C_template.t Loc.t
val alloc : t -> bool
val of_expression : expression -> t option
val native_and_bytecode_differ : t -> bool
