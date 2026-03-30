open! Core
open! Ppxlib
open! Import

(** type annotations supported in c_expression.s *)
type t =
  | Int (** : int *)
  | Int32 (** : Int32.t *)
  | Int64 (** : Int64.t *)
  | Float (** : float *)
  | Value of string (** : SOMETHING value *)
  | Local_value of string (** : SOMETHING local_value *)
[@@deriving compare ~localize, sexp_of]

val to_ocaml_type : t -> core_type * Ppxlib_jane.modes
val to_c_type : t -> string
val of_string : string -> t
val of_core_type : core_type -> t
val is_unboxed_or_untagged : t -> bool
val pack_allocates : t -> bool
