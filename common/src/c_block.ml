open! Core
open! Ppxlib
open! Import

type t =
  | Top_level of string
  | Type_def of C_type_def.t
  | Expression of C_expression.t

let fold_map (type acc) ~path ~top_level ~type_def ~expression =
  let rec make_walker ~path =
    object
      inherit [acc] Ast.fold_map as super
      method string x acc = x, acc
      method int x acc = x, acc
      method char x acc = x, acc
      method bool x acc = x, acc

      method option f x acc =
        match x with
        | None -> None, acc
        | Some x ->
          let x, acc = f x acc in
          Some x, acc

      method list f x init =
        List.fold_map x ~init ~f:(fun acc x -> f x acc |> Tuple2.swap) |> Tuple2.swap

      method! structure_item stri acc =
        let loc = stri.pstr_loc in
        match stri with
        | [%stri [%%c [%e? code]]] ->
          let { loc; txt } = long_string_constant code in
          top_level acc ~loc ~path txt
        | _ ->
          (match C_type_def.of_structure_item stri with
           | Some i -> type_def acc ~loc ~path i
           | None -> super#structure_item stri acc)

      method! expression expr acc =
        let loc = expr.pexp_loc in
        match C_expression.of_expression expr with
        | None -> super#expression expr acc
        | Some code -> expression acc ~loc ~path code

      method! module_binding mod_ acc =
        match mod_.pmb_name.txt with
        | Some name ->
          let pmb_expr, acc =
            (make_walker ~path:(path @ [ name ]))#module_expr mod_.pmb_expr acc
          in
          { mod_ with pmb_expr }, acc
        | _ -> super#module_binding mod_ acc
    end
  in
  make_walker ~path
;;

let find_all str =
  (fold_map
     ~path:[]
     ~top_level:(fun acc ~loc ~path item ->
       [%stri let () = ()], (path, { loc; txt = Top_level item }) :: acc)
     ~type_def:(fun acc ~loc ~path item ->
       [%stri let () = ()], (path, { loc; txt = Type_def item }) :: acc)
     ~expression:(fun acc ~loc ~path item ->
       [%expr ()], (path, { loc; txt = Expression item }) :: acc))
    #structure
    str
    []
  |> snd
  |> List.rev
;;

let map_struct ~top_level ~type_def ~expression str =
  (fold_map
     ~path:[]
     ~top_level:(fun () ~loc ~path item -> top_level ~loc ~path item, ())
     ~type_def:(fun () ~loc ~path item -> type_def ~loc ~path item, ())
     ~expression:(fun () ~loc ~path item -> expression ~loc ~path item, ()))
    #structure
    str
    ()
  |> fst
;;
