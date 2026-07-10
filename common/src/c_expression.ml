open! Core
open! Ppxlib
open! Import

type t =
  { code : C_template.t Loc.t
  ; args : Type_.t String.Map.t
  ; return : Type_.t option
  ; alloc : bool
  }
[@@deriving sexp_of, fields ~getters]

let unique_name = unique_name ~kind:"expr" ~sexp_of_t

let of_expression expr =
  let%map.Option alloc, expr =
    match expr with
    | [%expr [%c.alloc [%e? expr]]] -> Some (true, expr)
    | [%expr [%c.no_alloc [%e? expr]]] -> Some (false, expr)
    | _ -> None
  in
  let code, return =
    match expr with
    | [%expr ([%e? code] : [%t? return])] -> code, Some (Type_.of_core_type return)
    | _ -> expr, None
  in
  let code = long_string_constant code |> Loc.map ~f:C_template.of_string in
  let args = C_template.vars code.txt in
  { code; args; return; alloc }
;;
