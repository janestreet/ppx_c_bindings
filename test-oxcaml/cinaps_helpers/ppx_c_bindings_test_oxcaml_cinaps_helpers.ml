open! Core

let find_comment_end code ~pos =
  assert (String.is_substring_at code ~pos ~substring:"(*");
  let pos = pos + 2 in
  let next ~pos =
    [ `Open, "(*"; `Close, "*)" ]
    |> List.filter_map ~f:(fun (tag, pattern) ->
      String.substr_index code ~pos ~pattern
      |> Option.map ~f:(fun pos -> pos + String.length pattern, tag))
    |> List.min_elt ~compare:[%compare: int * _]
    |> Option.value_exn
  in
  let rec loop ~pos ~depth =
    if depth = 0
    then pos
    else (
      let pos, tag = next ~pos in
      match tag with
      | `Open -> loop ~depth:(depth + 1) ~pos
      | `Close -> loop ~depth:(depth - 1) ~pos)
  in
  loop ~pos ~depth:1
;;

let strip_special_comments ~marker code =
  let rec loop ~pos =
    match String.substr_index code ~pos ~pattern:marker with
    | None -> [ String.subo code ~pos ]
    | Some start_pos ->
      let end_pos = find_comment_end code ~pos:start_pos in
      String.subo code ~pos ~len:(start_pos - pos) :: loop ~pos:end_pos
  in
  loop ~pos:0 |> String.concat ~sep:"\n"
;;

let include_file ~file rewrites =
  let file =
    In_channel.read_all file
    |> strip_special_comments ~marker:"(*$"
    |> strip_special_comments ~marker:"(* CR"
    |> strip_special_comments ~marker:"(* XCR"
    (* disable any existing cinaps code blocks - they would be expanded in the source *)
  in
  List.fold rewrites ~init:file ~f:(fun file (pattern, with_) ->
    String.substr_replace_all file ~pattern ~with_)
  |> print_endline
;;
