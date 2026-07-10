open! Core

(*$
  open! Core

  [%%template
  [@@@kind k = (base, bits8, bits16)]

  module type N = sig
    val ocaml_type : string

    type t [@@deriving sexp_of]

    val num_bits : t
    val zero : t
    val one : t
    val min_value : t
    val max_value : t
    val of_int_exn : int -> t
    val to_int_trunc : t -> int
  end
  [@@kind k] [@@disable_unused_warnings]

  let[@kind k] print_number_tests
    ?(fix_no_alloc_unsigned_gets_sign_extended = [])
    (module N : N[@kind k])
    =
    let module N = struct
      include N

      let to_string t = Sexp.to_string (N.sexp_of_t t)
    end
    in
    let pct = "%" in
    let ocaml_type = N.ocaml_type in
    let ocaml_module =
      match String.rsplit2 ~on:'.' ocaml_type with
      | None -> String.capitalize ocaml_type
      | Some (module_, "t") -> module_
      | Some (module_, type_) -> [%string {|%{module_}.%{String.capitalize type_}|}]
    in
    let c_name = Ppx_c_bindings_common.For_testing.Type_.ocaml_to_c ocaml_type in
    let module Sample_value = struct
      type t =
        { value : N.t
        ; ox_expr : string
        }
    end
    in
    List.iter
      [ "alloc", "CAMLreturn"; "no_alloc", "return" ]
      ~f:(fun (alloc, return) ->
        let to_int n =
          let n = N.to_int_trunc n in
          match alloc with
          | "no_alloc" ->
            (* For some reasons the no_alloc C linkage ends up sign extending the unsigned
               numbers... *)
            List.Assoc.find fix_no_alloc_unsigned_gets_sign_extended ~equal:Int.equal n
            |> Option.value ~default:n
          | _ -> n
        in
        let var ~ocaml_type name = [%string {|%{pct}{%{name}:%{ocaml_type}}|}] in
        print_endline
          [%string
            {ocaml|
      let%expect_test "%{ocaml_type} (%{alloc})" =
        let to_int (n:%{ocaml_type}) = [%c.%{alloc} ({| %{return} (%{pct}{n:%{ocaml_type}}); |} : int)] in
            |ocaml}];
        [ { Sample_value.value = N.zero; ox_expr = [%string {|%{ocaml_module}.zero|}] }
        ; { Sample_value.value = N.one; ox_expr = [%string {|%{ocaml_module}.one|}] }
        ]
        @ List.filter_map
            [ -1
            ; 0x7f
            ; 0x80
            ; 0xff
            ; 0x7fff
            ; 0x8000
            ; 0xffff
            ; 0x7fff_ffff
            ; 0x8000_0000
            ; 0xffff_ffff
            ; 42
            ; -42
            ]
            ~f:(fun n ->
              Option.try_with (fun () ->
                { Sample_value.value = N.of_int_exn n
                ; ox_expr = [%string {|(%{ocaml_module}.of_int_exn (%{n#Int}))|}]
                }))
        @ [ { Sample_value.value = N.min_value
            ; ox_expr = [%string {|%{ocaml_module}.min_value|}]
            }
          ; { Sample_value.value = N.max_value
            ; ox_expr = [%string {|%{ocaml_module}.max_value|}]
            }
          ]
        |> List.iter ~f:(fun { Sample_value.value; ox_expr } ->
          print_endline
            [%string
              {ocaml|
  print_s [%sexp ([%c.%{alloc} ({| %{return} (%{value#N}); |} : %{ocaml_type})] : %{ocaml_type})];
  [%expect {| %{value#N} |}];
  print_s [%sexp (to_int %{ox_expr} : int)];
  [%expect {| %{to_int value#Int} |}];
        |ocaml}]);
        print_endline
          [%string
            {ocaml|
        ()
      ;;
      |ocaml}]);
    let is_signed =
      match N.of_int_exn (-1) with
      | _ -> true
      | exception _ -> false
    in
    (* allows negative values *)
    let all_fs =
      match N.to_int_trunc N.num_bits with
      | 8 -> "0xff"
      | 16 -> "0xffff"
      | 31 | 32 -> "0xffffffff" (* 31 bits gets treated as 32 bit on the C side *)
      | 63 | 64 -> "0xffffffffffffffff" (* 63 bits gets treated as 64 bit on C side *)
      | bits -> raise_s [%message "TODO: add missing bit width to [all_fs]" (bits : int)]
    in
    print_endline
      [%string
        {ocaml|
let%expect_test "%{ocaml_type} check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare = 
    %{ocaml_module}.compare %{ocaml_module}.zero [%c.no_alloc ({| return %{all_fs}; |} : %{ocaml_type})] > 0
  in
  let signed_from_c_compare = 
     [%c.no_alloc ({| return (((%{c_name})0) > ((%{c_name})%{all_fs})); |} : int)] <> 0
  in
  print_s [%message "" 
          (signed_from_ocaml_compare : bool)
          (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare %{is_signed#Bool}) (signed_from_c_compare %{is_signed#Bool})) |}];
  print_s [%message "" ~all_fs:([%c.no_alloc ({| return %{all_fs}; |} : %{ocaml_type})]:%{ocaml_type})];
  [%expect {| (all_fs %{if is_signed then "-1" else N.to_string N.max_value}) |}]
|ocaml}];
    ()
  [@@disable_unused_warnings]
  ;;]

  let () =
    let not_an_integer_type _ = () in
    Ppx_c_bindings_common.For_testing.Type_.iter
      ~int:(fun _ ->
        (print_number_tests [@kind value])
          (module struct
            include Int

            let ocaml_type = "int"
            let to_int_trunc = Fn.id
          end))
      ~int32:(fun _ ->
        (print_number_tests [@kind value])
          (module struct
            include Int32

            let ocaml_type = "Int32.t"
          end))
      ~int64:(fun _ ->
        (print_number_tests [@kind value])
          (module struct
            include Int64

            let ocaml_type = "Int64.t"
          end))
      ~float:not_an_integer_type
      ~value:not_an_integer_type
      ~local_value:not_an_integer_type
      ~u8:(fun _ ->
        (print_number_tests [@kind bits8])
          (module struct
            include Ox.U8

            let ocaml_type = "Ox.u8"
            let to_int_trunc = to_int
          end)
          ~fix_no_alloc_unsigned_gets_sign_extended:
            [ 0x80, 0xffff_ff80; 0xff, 0xffff_ffff ])
      ~i8:(fun _ ->
        (print_number_tests [@kind bits8])
          (module struct
            include Ox.I8

            let ocaml_type = "Ox.i8"
            let to_int_trunc = to_int
          end))
      ~u16:(fun _ ->
        (print_number_tests [@kind bits16])
          (module struct
            include Ox.U16

            let ocaml_type = "Ox.u16"
            let to_int_trunc = to_int
          end)
          ~fix_no_alloc_unsigned_gets_sign_extended:
            [ 0x8000, 0xffff_8000; 0xffff, 0xffff_ffff ])
      ~i16:(fun _ ->
        (print_number_tests [@kind bits16])
          (module struct
            include Ox.I16

            let ocaml_type = "Ox.i16"
            let to_int_trunc = to_int
          end))
      ~u32:(fun _ ->
        (print_number_tests [@kind bits32])
          (module struct
            include Ox.U32

            let ocaml_type = "Ox.u32"
          end))
      ~i32:(fun _ ->
        (print_number_tests [@kind bits32])
          (module struct
            include Ox.I32

            let ocaml_type = "Ox.i32"
          end))
      ~u64:(fun _ ->
        (print_number_tests [@kind bits64])
          (module struct
            include Ox.U64

            let ocaml_type = "Ox.u64"
            let to_int_trunc n = n |> Ox.U64.to_i64_wrap |> Ox.I64.to_int_trunc
          end))
      ~i64:(fun _ ->
        (print_number_tests [@kind bits64])
          (module struct
            include Ox.I64

            let ocaml_type = "Ox.i64"
          end))
      ~f32:not_an_integer_type
      ~f64:not_an_integer_type
      ~isize:(fun _ ->
        (print_number_tests [@kind word])
          (module struct
            include Ox.Isize

            let ocaml_type = "Ox.isize"
          end))
      ~mem:not_an_integer_type
      ~ptr_ext:not_an_integer_type
      ~ptr_ext_imm:not_an_integer_type
      ~addr_ext:not_an_integer_type
      ~addr_ext_imm:not_an_integer_type
      ~unboxed_tuple:not_an_integer_type
  ;;
*)
let%expect_test "int (alloc)" =
  let to_int (n : int) = [%c.alloc ({| CAMLreturn (%{n:int}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : int)] : int)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : int)] : int)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : int)] : int)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : int)] : int)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : int)] : int)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : int)] : int)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : int)] : int)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : int)] : int)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : int)] : int)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : int)] : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : int)] : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Int.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : int)] : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Int.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : int)] : int)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : int)] : int)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-4611686018427387904); |} : int)] : int)];
  [%expect {| -4611686018427387904 |}];
  print_s [%sexp (to_int Int.min_value : int)];
  [%expect {| -4611686018427387904 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4611686018427387903); |} : int)] : int)];
  [%expect {| 4611686018427387903 |}];
  print_s [%sexp (to_int Int.max_value : int)];
  [%expect {| 4611686018427387903 |}];
  ()
;;

let%expect_test "int (no_alloc)" =
  let to_int (n : int) = [%c.no_alloc ({| return (%{n:int}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : int)] : int)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : int)] : int)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : int)] : int)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : int)] : int)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : int)] : int)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : int)] : int)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : int)] : int)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : int)] : int)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : int)] : int)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : int)] : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : int)] : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Int.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : int)] : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Int.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : int)] : int)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : int)] : int)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-4611686018427387904); |} : int)] : int)];
  [%expect {| -4611686018427387904 |}];
  print_s [%sexp (to_int Int.min_value : int)];
  [%expect {| -4611686018427387904 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4611686018427387903); |} : int)] : int)];
  [%expect {| 4611686018427387903 |}];
  print_s [%sexp (to_int Int.max_value : int)];
  [%expect {| 4611686018427387903 |}];
  ()
;;

let%expect_test "int check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Int.compare Int.zero [%c.no_alloc ({| return 0xffffffffffffffff; |} : int)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((intnat)0) > ((intnat)0xffffffffffffffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message "" ~all_fs:([%c.no_alloc ({| return 0xffffffffffffffff; |} : int)] : int)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Int32.t (alloc)" =
  let to_int (n : Int32.t) = [%c.alloc ({| CAMLreturn (%{n:Int32.t}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Int32.t)] : Int32.t)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Int32.t)] : Int32.t)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Int32.t)] : Int32.t)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int32.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Int32.t)] : Int32.t)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Int32.t)] : Int32.t)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Int32.t)] : Int32.t)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Int32.t)] : Int32.t)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Int32.t)] : Int32.t)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Int32.t)] : Int32.t)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Int32.t)] : Int32.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Int32.t)] : Int32.t)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Int32.t)] : Int32.t)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int32.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-2147483648); |} : Int32.t)] : Int32.t)];
  [%expect {| -2147483648 |}];
  print_s [%sexp (to_int Int32.min_value : int)];
  [%expect {| -2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Int32.t)] : Int32.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int Int32.max_value : int)];
  [%expect {| 2147483647 |}];
  ()
;;

let%expect_test "Int32.t (no_alloc)" =
  let to_int (n : Int32.t) = [%c.no_alloc ({| return (%{n:Int32.t}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Int32.t)] : Int32.t)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Int32.t)] : Int32.t)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Int32.t)] : Int32.t)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int32.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Int32.t)] : Int32.t)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Int32.t)] : Int32.t)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Int32.t)] : Int32.t)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Int32.t)] : Int32.t)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Int32.t)] : Int32.t)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Int32.t)] : Int32.t)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Int32.t)] : Int32.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Int32.t)] : Int32.t)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Int32.t)] : Int32.t)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int32.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-2147483648); |} : Int32.t)] : Int32.t)];
  [%expect {| -2147483648 |}];
  print_s [%sexp (to_int Int32.min_value : int)];
  [%expect {| -2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Int32.t)] : Int32.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int Int32.max_value : int)];
  [%expect {| 2147483647 |}];
  ()
;;

let%expect_test "Int32.t check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Int32.compare Int32.zero [%c.no_alloc ({| return 0xffffffff; |} : Int32.t)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int32_t)0) > ((int32_t)0xffffffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message "" ~all_fs:([%c.no_alloc ({| return 0xffffffff; |} : Int32.t)] : Int32.t)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Int64.t (alloc)" =
  let to_int (n : Int64.t) = [%c.alloc ({| CAMLreturn (%{n:Int64.t}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Int64.t)] : Int64.t)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Int64.t)] : Int64.t)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Int64.t)] : Int64.t)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int64.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Int64.t)] : Int64.t)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Int64.t)] : Int64.t)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Int64.t)] : Int64.t)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Int64.t)] : Int64.t)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Int64.t)] : Int64.t)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Int64.t)] : Int64.t)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Int64.t)] : Int64.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : Int64.t)] : Int64.t)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Int64.t)] : Int64.t)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Int64.t)] : Int64.t)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Int64.t)] : Int64.t)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int64.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (-9223372036854775808); |} : Int64.t)] : Int64.t)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Int64.min_value : int)];
  [%expect {| 0 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (9223372036854775807); |} : Int64.t)] : Int64.t)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Int64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Int64.t (no_alloc)" =
  let to_int (n : Int64.t) = [%c.no_alloc ({| return (%{n:Int64.t}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Int64.t)] : Int64.t)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Int64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Int64.t)] : Int64.t)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Int64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Int64.t)] : Int64.t)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Int64.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Int64.t)] : Int64.t)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Int64.t)] : Int64.t)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Int64.t)] : Int64.t)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Int64.t)] : Int64.t)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Int64.t)] : Int64.t)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Int64.t)] : Int64.t)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Int64.t)] : Int64.t)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : Int64.t)] : Int64.t)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Int64.t)] : Int64.t)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Int64.t)] : Int64.t)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Int64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Int64.t)] : Int64.t)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Int64.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s
    [%sexp ([%c.no_alloc ({| return (-9223372036854775808); |} : Int64.t)] : Int64.t)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Int64.min_value : int)];
  [%expect {| 0 |}];
  print_s
    [%sexp ([%c.no_alloc ({| return (9223372036854775807); |} : Int64.t)] : Int64.t)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Int64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Int64.t check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Int64.compare Int64.zero [%c.no_alloc ({| return 0xffffffffffffffff; |} : Int64.t)]
    > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int64_t)0) > ((int64_t)0xffffffffffffffff)); |} : int)]
    <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message
      "" ~all_fs:([%c.no_alloc ({| return 0xffffffffffffffff; |} : Int64.t)] : Int64.t)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Ox.u8 (alloc)" =
  let to_int (n : Ox.u8) = [%c.alloc ({| CAMLreturn (%{n:Ox.u8}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U8.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U8.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U8.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int Ox.U8.max_value : int)];
  [%expect {| 255 |}];
  ()
;;

let%expect_test "Ox.u8 (no_alloc)" =
  let to_int (n : Ox.u8) = [%c.no_alloc ({| return (%{n:Ox.u8}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U8.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U8.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 128) : int)];
  [%expect {| 4294967168 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 255) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U8.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U8.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.u8)] : Ox.u8)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int Ox.U8.max_value : int)];
  [%expect {| 4294967295 |}];
  ()
;;

let%expect_test "Ox.u8 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.U8.compare Ox.U8.zero [%c.no_alloc ({| return 0xff; |} : Ox.u8)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((uint8_t)0) > ((uint8_t)0xff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare false) (signed_from_c_compare false)) |}];
  print_s [%message "" ~all_fs:([%c.no_alloc ({| return 0xff; |} : Ox.u8)] : Ox.u8)];
  [%expect {| (all_fs 255) |}]
;;

let%expect_test "Ox.i8 (alloc)" =
  let to_int (n : Ox.i8) = [%c.alloc ({| CAMLreturn (%{n:Ox.i8}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I8.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I8.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-128); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -128 |}];
  print_s [%sexp (to_int Ox.I8.min_value : int)];
  [%expect {| -128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int Ox.I8.max_value : int)];
  [%expect {| 127 |}];
  ()
;;

let%expect_test "Ox.i8 (no_alloc)" =
  let to_int (n : Ox.i8) = [%c.no_alloc ({| return (%{n:Ox.i8}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I8.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I8.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I8.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-128); |} : Ox.i8)] : Ox.i8)];
  [%expect {| -128 |}];
  print_s [%sexp (to_int Ox.I8.min_value : int)];
  [%expect {| -128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.i8)] : Ox.i8)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int Ox.I8.max_value : int)];
  [%expect {| 127 |}];
  ()
;;

let%expect_test "Ox.i8 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.I8.compare Ox.I8.zero [%c.no_alloc ({| return 0xff; |} : Ox.i8)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int8_t)0) > ((int8_t)0xff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s [%message "" ~all_fs:([%c.no_alloc ({| return 0xff; |} : Ox.i8)] : Ox.i8)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Ox.u16 (alloc)" =
  let to_int (n : Ox.u16) = [%c.alloc ({| CAMLreturn (%{n:Ox.u16}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U16.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U16.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U16.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int Ox.U16.max_value : int)];
  [%expect {| 65535 |}];
  ()
;;

let%expect_test "Ox.u16 (no_alloc)" =
  let to_int (n : Ox.u16) = [%c.no_alloc ({| return (%{n:Ox.u16}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U16.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U16.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 32768) : int)];
  [%expect {| 4294934528 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 65535) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U16.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U16.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.u16)] : Ox.u16)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int Ox.U16.max_value : int)];
  [%expect {| 4294967295 |}];
  ()
;;

let%expect_test "Ox.u16 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.U16.compare Ox.U16.zero [%c.no_alloc ({| return 0xffff; |} : Ox.u16)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((uint16_t)0) > ((uint16_t)0xffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare false) (signed_from_c_compare false)) |}];
  print_s [%message "" ~all_fs:([%c.no_alloc ({| return 0xffff; |} : Ox.u16)] : Ox.u16)];
  [%expect {| (all_fs 65535) |}]
;;

let%expect_test "Ox.i16 (alloc)" =
  let to_int (n : Ox.i16) = [%c.alloc ({| CAMLreturn (%{n:Ox.i16}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I16.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I16.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-32768); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -32768 |}];
  print_s [%sexp (to_int Ox.I16.min_value : int)];
  [%expect {| -32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int Ox.I16.max_value : int)];
  [%expect {| 32767 |}];
  ()
;;

let%expect_test "Ox.i16 (no_alloc)" =
  let to_int (n : Ox.i16) = [%c.no_alloc ({| return (%{n:Ox.i16}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I16.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I16.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I16.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-32768); |} : Ox.i16)] : Ox.i16)];
  [%expect {| -32768 |}];
  print_s [%sexp (to_int Ox.I16.min_value : int)];
  [%expect {| -32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.i16)] : Ox.i16)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int Ox.I16.max_value : int)];
  [%expect {| 32767 |}];
  ()
;;

let%expect_test "Ox.i16 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.I16.compare Ox.I16.zero [%c.no_alloc ({| return 0xffff; |} : Ox.i16)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int16_t)0) > ((int16_t)0xffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s [%message "" ~all_fs:([%c.no_alloc ({| return 0xffff; |} : Ox.i16)] : Ox.i16)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Ox.u32 (alloc)" =
  let to_int (n : Ox.u32) = [%c.alloc ({| CAMLreturn (%{n:Ox.u32}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U32.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int Ox.U32.max_value : int)];
  [%expect {| 4294967295 |}];
  ()
;;

let%expect_test "Ox.u32 (no_alloc)" =
  let to_int (n : Ox.u32) = [%c.no_alloc ({| return (%{n:Ox.u32}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U32.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Ox.u32)] : Ox.u32)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int Ox.U32.max_value : int)];
  [%expect {| 4294967295 |}];
  ()
;;

let%expect_test "Ox.u32 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.U32.compare Ox.U32.zero [%c.no_alloc ({| return 0xffffffff; |} : Ox.u32)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((uint32_t)0) > ((uint32_t)0xffffffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare false) (signed_from_c_compare false)) |}];
  print_s
    [%message "" ~all_fs:([%c.no_alloc ({| return 0xffffffff; |} : Ox.u32)] : Ox.u32)];
  [%expect {| (all_fs 4294967295) |}]
;;

let%expect_test "Ox.i32 (alloc)" =
  let to_int (n : Ox.i32) = [%c.alloc ({| CAMLreturn (%{n:Ox.i32}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-2147483648); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -2147483648 |}];
  print_s [%sexp (to_int Ox.I32.min_value : int)];
  [%expect {| -2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int Ox.I32.max_value : int)];
  [%expect {| 2147483647 |}];
  ()
;;

let%expect_test "Ox.i32 (no_alloc)" =
  let to_int (n : Ox.i32) = [%c.no_alloc ({| return (%{n:Ox.i32}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I32.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I32.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I32.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-2147483648); |} : Ox.i32)] : Ox.i32)];
  [%expect {| -2147483648 |}];
  print_s [%sexp (to_int Ox.I32.min_value : int)];
  [%expect {| -2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.i32)] : Ox.i32)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int Ox.I32.max_value : int)];
  [%expect {| 2147483647 |}];
  ()
;;

let%expect_test "Ox.i32 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.I32.compare Ox.I32.zero [%c.no_alloc ({| return 0xffffffff; |} : Ox.i32)] > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int32_t)0) > ((int32_t)0xffffffff)); |} : int)] <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message "" ~all_fs:([%c.no_alloc ({| return 0xffffffff; |} : Ox.i32)] : Ox.i32)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Ox.u64 (alloc)" =
  let to_int (n : Ox.u64) = [%c.alloc ({| CAMLreturn (%{n:Ox.u64}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U64.min_value : int)];
  [%expect {| 0 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (18446744073709551615); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 18446744073709551615 |}];
  print_s [%sexp (to_int Ox.U64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.u64 (no_alloc)" =
  let to_int (n : Ox.u64) = [%c.no_alloc ({| return (%{n:Ox.u64}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.U64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.U64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.U64.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (18446744073709551615); |} : Ox.u64)] : Ox.u64)];
  [%expect {| 18446744073709551615 |}];
  print_s [%sexp (to_int Ox.U64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.u64 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.U64.compare Ox.U64.zero [%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.u64)]
    > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((uint64_t)0) > ((uint64_t)0xffffffffffffffff)); |} : int)]
    <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare false) (signed_from_c_compare false)) |}];
  print_s
    [%message
      "" ~all_fs:([%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.u64)] : Ox.u64)];
  [%expect {| (all_fs 18446744073709551615) |}]
;;

let%expect_test "Ox.i64 (alloc)" =
  let to_int (n : Ox.i64) = [%c.alloc ({| CAMLreturn (%{n:Ox.i64}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (-9223372036854775808); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Ox.I64.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (9223372036854775807); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Ox.I64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.i64 (no_alloc)" =
  let to_int (n : Ox.i64) = [%c.no_alloc ({| return (%{n:Ox.i64}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.I64.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.I64.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.I64.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-9223372036854775808); |} : Ox.i64)] : Ox.i64)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Ox.I64.min_value : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (9223372036854775807); |} : Ox.i64)] : Ox.i64)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Ox.I64.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.i64 check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.I64.compare Ox.I64.zero [%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.i64)]
    > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((int64_t)0) > ((int64_t)0xffffffffffffffff)); |} : int)]
    <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message
      "" ~all_fs:([%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.i64)] : Ox.i64)];
  [%expect {| (all_fs -1) |}]
;;

let%expect_test "Ox.isize (alloc)" =
  let to_int (n : Ox.isize) = [%c.alloc ({| CAMLreturn (%{n:Ox.isize}); |} : int)] in
  print_s [%sexp ([%c.alloc ({| CAMLreturn (0); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.Isize.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (1); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.Isize.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-1); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (127); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (128); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (255); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32767); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (32768); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (65535); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483647); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (2147483648); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (4294967295); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (42); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.alloc ({| CAMLreturn (-42); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (-9223372036854775808); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Ox.Isize.min_value : int)];
  [%expect {| 0 |}];
  print_s
    [%sexp ([%c.alloc ({| CAMLreturn (9223372036854775807); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Ox.Isize.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.isize (no_alloc)" =
  let to_int (n : Ox.isize) = [%c.no_alloc ({| return (%{n:Ox.isize}); |} : int)] in
  print_s [%sexp ([%c.no_alloc ({| return (0); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 0 |}];
  print_s [%sexp (to_int Ox.Isize.zero : int)];
  [%expect {| 0 |}];
  print_s [%sexp ([%c.no_alloc ({| return (1); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 1 |}];
  print_s [%sexp (to_int Ox.Isize.one : int)];
  [%expect {| 1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-1); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -1 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn (-1)) : int)];
  [%expect {| -1 |}];
  print_s [%sexp ([%c.no_alloc ({| return (127); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 127 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 127) : int)];
  [%expect {| 127 |}];
  print_s [%sexp ([%c.no_alloc ({| return (128); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 128 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 128) : int)];
  [%expect {| 128 |}];
  print_s [%sexp ([%c.no_alloc ({| return (255); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 255 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 255) : int)];
  [%expect {| 255 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32767); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 32767 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 32767) : int)];
  [%expect {| 32767 |}];
  print_s [%sexp ([%c.no_alloc ({| return (32768); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 32768 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 32768) : int)];
  [%expect {| 32768 |}];
  print_s [%sexp ([%c.no_alloc ({| return (65535); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 65535 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 65535) : int)];
  [%expect {| 65535 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483647); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 2147483647 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 2147483647) : int)];
  [%expect {| 2147483647 |}];
  print_s [%sexp ([%c.no_alloc ({| return (2147483648); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 2147483648 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 2147483648) : int)];
  [%expect {| 2147483648 |}];
  print_s [%sexp ([%c.no_alloc ({| return (4294967295); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 4294967295 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 4294967295) : int)];
  [%expect {| 4294967295 |}];
  print_s [%sexp ([%c.no_alloc ({| return (42); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 42 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn 42) : int)];
  [%expect {| 42 |}];
  print_s [%sexp ([%c.no_alloc ({| return (-42); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -42 |}];
  print_s [%sexp (to_int (Ox.Isize.of_int_exn (-42)) : int)];
  [%expect {| -42 |}];
  print_s
    [%sexp ([%c.no_alloc ({| return (-9223372036854775808); |} : Ox.isize)] : Ox.isize)];
  [%expect {| -9223372036854775808 |}];
  print_s [%sexp (to_int Ox.Isize.min_value : int)];
  [%expect {| 0 |}];
  print_s
    [%sexp ([%c.no_alloc ({| return (9223372036854775807); |} : Ox.isize)] : Ox.isize)];
  [%expect {| 9223372036854775807 |}];
  print_s [%sexp (to_int Ox.Isize.max_value : int)];
  [%expect {| -1 |}];
  ()
;;

let%expect_test "Ox.isize check ocaml and C agree on signed-ness" =
  let signed_from_ocaml_compare =
    Ox.Isize.compare
      Ox.Isize.zero
      [%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.isize)]
    > 0
  in
  let signed_from_c_compare =
    [%c.no_alloc ({| return (((ssize_t)0) > ((ssize_t)0xffffffffffffffff)); |} : int)]
    <> 0
  in
  print_s [%message "" (signed_from_ocaml_compare : bool) (signed_from_c_compare : bool)];
  [%expect {| ((signed_from_ocaml_compare true) (signed_from_c_compare true)) |}];
  print_s
    [%message
      "" ~all_fs:([%c.no_alloc ({| return 0xffffffffffffffff; |} : Ox.isize)] : Ox.isize)];
  [%expect {| (all_fs -1) |}]
;;

(*$*)
