open! Core
open! Ppxlib
open! Import

type elt =
  | Text of string
  | Var of
      { name : string
      ; type_ : Type_.t option
      }
[@@deriving sexp_of]

type t = elt list [@@deriving sexp_of]

let of_string str =
  let rec parse_text pos =
    if String.length str <= pos
    then []
    else (
      match String.substr_index str ~pos ~pattern:"%{" with
      | None -> [ Text (String.subo str ~pos) ]
      | Some start_var_pos ->
        if pos = start_var_pos
        then parse_var (start_var_pos + 2)
        else
          Text (String.sub str ~pos ~len:(start_var_pos - pos))
          :: parse_var (start_var_pos + 2))
  and parse_var pos =
    let end_pos = String.substr_index_exn str ~pos ~pattern:"}" in
    let colon_pos =
      String.substr_index str ~pos ~pattern:":" |> Option.filter ~f:(( > ) end_pos)
    in
    let name =
      let len = Option.value colon_pos ~default:end_pos - pos in
      String.strip (String.sub str ~pos ~len)
    in
    let type_ =
      Option.map colon_pos ~f:(fun pos ->
        let pos = pos + 1 in
        let len = end_pos - pos in
        String.sub str ~pos ~len |> Type_.of_string)
    in
    Var { name; type_ } :: parse_text (end_pos + 1)
  in
  parse_text 0
;;

let%expect_test _ =
  let test str = str |> of_string |> printf !"%{sexp:t}" in
  test {|hello|};
  [%expect {| ((Text hello)) |}];
  test {|sin(%{a:float})|};
  [%expect {| ((Text "sin(") (Var (name a) (type_ (Float))) (Text ")")) |}]
;;

let to_string ~var_to_string ts =
  List.map ts ~f:(function
    | Var { name; type_ } -> var_to_string ?type_ name
    | Text txt -> txt)
  |> String.concat
;;

let vars t =
  t
  |> List.fold ~init:String.Map.empty ~f:(fun vars -> function
    | Text _ -> vars
    | Var { name; type_ } ->
      Map.update vars name ~f:(function
        | None -> type_
        | Some type_' ->
          Option.merge type_ type_' ~f:(fun a b ->
            if [%compare.equal: Type_.t] a b
            then a
            else
              failwith
                [%string
                  "%{name} has conflicting types %{Type_.sexp_of_t a#Sexp} <> \
                   %{Type_.sexp_of_t b#Sexp}"])))
  |> Map.mapi ~f:(fun ~key ~data ->
    match data with
    | None -> failwith [%string "%{key} is of unknown type"]
    | Some type_ -> type_)
;;

let is_substring t ~substring =
  List.exists t ~f:(function
    | Text t -> String.is_substring t ~substring
    | Var _ -> false)
;;
