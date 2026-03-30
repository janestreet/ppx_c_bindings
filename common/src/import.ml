open! Core
open! Ppxlib

module Location = struct
  include Location

  let compare _ _ = 0
  let sexp_of_t _ = [%sexp "_"]
end

module Loc = struct
  include Loc

  let compare f a b = f a.txt b.txt
  let sexp_of_t sexp_of_a t = sexp_of_a t.txt
end

let long_string_constant = function
  | { pexp_desc = Pexp_constant (Pconst_string (txt, loc, Some _))
    ; pexp_loc = _
    ; pexp_loc_stack = _
    ; pexp_attributes = []
    } -> { txt; loc }
  | { pexp_loc = loc; _ } ->
    Ppxlib.Location.raise_errorf ~loc "Expected a string constant"
;;

let format_string f x =
  assert (Format.flush_str_formatter () |> String.is_empty);
  f Format.str_formatter x;
  Format.flush_str_formatter ()
;;

let parse_string p ?file str =
  let lexer = Lexing.from_string str in
  Option.iter file ~f:(Lexing.set_filename lexer);
  p lexer
;;

let unique_name ~kind ~sexp_of_t ~loc t =
  let filename =
    String.lsplit2 loc.loc_start.pos_fname ~on:'.'
    |> Option.value_map ~f:fst ~default:loc.loc_start.pos_fname
    |> Filename.basename
    |> String.uppercase
  in
  let digest =
    (* Random probably unique id to avoid name clashes or stale code *)
    t |> sexp_of_t |> Sexp.to_string_mach |> Crc.crc32hex
  in
  [%string "%{filename}_%{loc.loc_start.pos_lnum#Int}_%{kind}_%{digest}"]
;;

let sexp_of_core_type t = [%sexp (format_string Pprintast.core_type t : string)]
let compare_core_type a b = Comparable.lift Sexp.compare ~f:sexp_of_core_type a b

let sexp_of_variance : variance -> Sexp.t = function
  | Covariant -> [%sexp "covariant"]
  | Contravariant -> [%sexp "contravariant"]
  | NoVariance -> [%sexp "no variance"]
;;

let compare_variance a b = Comparable.lift Sexp.compare ~f:sexp_of_variance a b

let sexp_of_injectivity : injectivity -> Sexp.t = function
  | Injective -> [%sexp "injective"]
  | NoInjectivity -> [%sexp "no injectivity"]
;;

let compare_injectivity a b = Comparable.lift Sexp.compare ~f:sexp_of_injectivity a b
