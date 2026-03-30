open! Core
open! Ppxlib
open! Ppxlib_jane.Shim
open! Import

(** Helpers for defining custom types
    {[
      type foo = [%c {| C_TYPE |}]
      type bar = [%c {| C_TYPE* |} ~free:{| free( t* ); |}]
    ]} *)

type t [@@deriving sexp_of]

val unique_name : loc:Location.t -> t -> string
val name : t -> string Loc.t
val params : t -> (core_type * (variance * injectivity)) list
val cstrs : t -> (core_type * core_type * Location.t) list
val c_type : t -> string Loc.t
val free : t -> string Loc.t option
val jkind : t -> jkind_annotation option
val of_structure_item : structure_item -> t option
