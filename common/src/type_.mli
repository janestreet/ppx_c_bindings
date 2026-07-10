open! Core
open! Ppxlib
open! Import

(** type annotations supported in c_expression.s *)
type t [@@deriving compare, sexp_of]

val to_ocaml_type
  :  loc:Location.t
  -> t
  -> default_locality:[ `Local | `Global ]
  -> core_type * Ppxlib_jane.modes

val to_c_type : t -> [ `Primitive of string | `Struct of string ]
val of_string : string -> t
val of_core_type : core_type -> t

(** GC roots need special handling and registered with the runtime at the function start *)
val is_gc_root : loc:Location.t -> t -> bool

module For_testing : sig
  val ocaml_to_c : string -> string
  val is_supported_type : string -> bool

  val iter
    :  int:(t Variant.t -> unit)
    -> int32:(t Variant.t -> unit)
    -> int64:(t Variant.t -> unit)
    -> float:(t Variant.t -> unit)
    -> value:((Ppxlib.core_type -> t) Base.Variant.t -> unit)
    -> local_value:((Ppxlib.core_type -> t) Base.Variant.t -> unit)
    -> u8:(t Variant.t -> unit)
    -> i8:(t Variant.t -> unit)
    -> u16:(t Variant.t -> unit)
    -> i16:(t Variant.t -> unit)
    -> u32:(t Variant.t -> unit)
    -> i32:(t Variant.t -> unit)
    -> u64:(t Variant.t -> unit)
    -> i64:(t Variant.t -> unit)
    -> f32:(t Variant.t -> unit)
    -> f64:(t Variant.t -> unit)
    -> isize:(t Variant.t -> unit)
    -> mem:(t Variant.t -> unit)
    -> ptr_ext:((Ppxlib.core_type -> t) Variant.t -> unit)
    -> ptr_ext_imm:((Ppxlib.core_type -> t) Variant.t -> unit)
    -> addr_ext:((Ppxlib.core_type -> t) Variant.t -> unit)
    -> addr_ext_imm:((Ppxlib.core_type -> t) Variant.t -> unit)
    -> unboxed_tuple:(((string option * t) list -> t) Variant.t -> unit)
    -> unit
end
