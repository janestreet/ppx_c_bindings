open! Core
open! Ppxlib
open! Import

(* Simple templating language for C, specifically it handles
   {| %{NAME} |} and {| %{NAME:type} |}.
   Each NAME must have :TYPE at least once, and if :TYPE is given
   more than once it must match the other ocurences.
*)
type t [@@deriving sexp_of]

val of_string : string -> t
val to_string : var_to_string:(?type_:Type_.t -> string -> string) -> t -> string
val vars : t -> Type_.t String.Map.t
val is_substring : t -> substring:string -> bool
