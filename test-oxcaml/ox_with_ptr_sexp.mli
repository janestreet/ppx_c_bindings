open! Core

type u8 = Ox.u8 [@@deriving sexp_of]
type i8 = Ox.i8 [@@deriving sexp_of]
type u16 = Ox.u16 [@@deriving sexp_of]
type i16 = Ox.i16 [@@deriving sexp_of]
type u32 = Ox.u32 [@@deriving sexp_of]
type i32 = Ox.i32 [@@deriving sexp_of]
type u64 = Ox.u64 [@@deriving sexp_of]
type i64 = Ox.i64 [@@deriving sexp_of]
type f32 = Ox.f32 [@@deriving sexp_of]
type f64 = Ox.f64 [@@deriving sexp_of]
type isize = Ox.isize [@@deriving sexp_of]
type mem = Ox.mem [@@deriving sexp_of]

module I64 : sig
  type t = i64 [@@deriving sexp_of, to_string]
end

module Ptr : sig
  module Ext : sig
    type ('a : any) t = 'a Ox.Ptr.Ext.t [@@deriving sexp_of]

    module Imm : sig
      type ('a : any) t = 'a Ox.Ptr.Ext.Imm.t [@@deriving sexp_of]
    end
  end
end

module Addr : sig
  module Ext : sig
    type ('a : any) t = 'a Ox.Addr.Ext.t [@@deriving sexp_of]

    module Imm : sig
      type ('a : any) t = 'a Ox.Addr.Ext.Imm.t [@@deriving sexp_of]
    end
  end
end
