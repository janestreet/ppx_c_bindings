open! Core
open! Ppxlib
open! Import

type t = private
  | Top_level of string
  (** top level
      {[
        [%%c...]
      ]} *)
  | Type_def of C_type_def.t
  (** top level
      {[
        type foo = [%c...]
      ]} *)
  | Expression of C_expression.t
  (** expression level
      {[
        [%c...]
      ]} *)

val find_all : structure -> (string list * t Loc.t) list

val map_struct
  :  top_level:(loc:Location.t -> path:string list -> string -> structure_item)
  -> type_def:(loc:Location.t -> path:string list -> C_type_def.t -> structure_item)
  -> expression:(loc:Location.t -> path:string list -> C_expression.t -> expression)
  -> structure
  -> structure
