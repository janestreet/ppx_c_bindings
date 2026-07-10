open! Core

[%%c
  {|
    #include <string.h>
  |}]

(*$
  open! Core

  let print_pointer_tests ?type_ ~to_ptr name =
    let type_ = Option.value type_ ~default:name in
    let pct = "%" in
    let _ = pct in
    List.iter
      [ "alloc", "CAMLreturn"; "no_alloc", "return" ]
      ~f:(fun (alloc, return) ->
        print_endline
          [%string
            {oxcaml|
let%expect_test "%{name} (%{alloc})" =
  let is_null (t : %{type_})  = t |> %{to_ptr} |> Ox.Ptr.Ext.is_null in
  let t = [%c.%{alloc} ({| %{return} (NULL); |} : %{type_})] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.%{alloc} ({| %{return} ("Hello C!"); |} : %{type_})] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t = 
    let len = [%c.%{alloc} ({| %{return} (strlen(%{pct}{t:%{type_}})); |} : int)] in
    let get i =
      [%c.%{alloc} 
        ({|
           %{return} (Val_int(((const char* )%{pct}{t:%{type_}})[%{pct}{i:int}])); 
         |} : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;
              |oxcaml}])
  ;;

  let () =
    let not_a_pointer_type _ = () in
    Ppx_c_bindings_common.For_testing.Type_.iter
      ~int:not_a_pointer_type
      ~int32:not_a_pointer_type
      ~int64:not_a_pointer_type
      ~float:not_a_pointer_type
      ~value:not_a_pointer_type
      ~local_value:not_a_pointer_type
      ~u8:not_a_pointer_type
      ~i8:not_a_pointer_type
      ~u16:not_a_pointer_type
      ~i16:not_a_pointer_type
      ~u32:not_a_pointer_type
      ~i32:not_a_pointer_type
      ~u64:not_a_pointer_type
      ~i64:not_a_pointer_type
      ~f32:not_a_pointer_type
      ~f64:not_a_pointer_type
      ~isize:not_a_pointer_type
      ~mem:(fun _ -> print_pointer_tests "Ox.mem" ~to_ptr:"Ox.Mem.to_ptr")
      ~ptr_ext:(fun _ ->
        print_pointer_tests "Ox.Ptr.Ext.t" ~type_:"Ox.u8 Ox.Ptr.Ext.t" ~to_ptr:"Fn.id")
      ~ptr_ext_imm:(fun _ ->
        print_pointer_tests
          "Ox.Ptr.Ext.Imm.t"
          ~type_:"Ox.u8 Ox.Ptr.Ext.Imm.t"
          ~to_ptr:"Ox.Ptr.Ext.of_imm")
      ~addr_ext:(fun _ ->
        print_pointer_tests
          "Ox.Addr.Ext.t"
          ~type_:"Ox.u8 Ox.Addr.Ext.t"
          ~to_ptr:"Ox.Addr.Ext.to_ptr")
      ~addr_ext_imm:(fun _ ->
        print_pointer_tests
          "Ox.Addr.Ext.Imm.t"
          ~type_:"Ox.u8 Ox.Addr.Ext.Imm.t"
          ~to_ptr:"Ox.Addr.Ext.Imm.to_ptr |> Ox.Ptr.Ext.of_imm")
      ~unboxed_tuple:not_a_pointer_type
  ;;
*)
let%expect_test "Ox.mem (alloc)" =
  let is_null (t : Ox.mem) = t |> Ox.Mem.to_ptr |> Ox.Ptr.Ext.is_null in
  let t = [%c.alloc ({| CAMLreturn (NULL); |} : Ox.mem)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.alloc ({| CAMLreturn ("Hello C!"); |} : Ox.mem)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.alloc ({| CAMLreturn (strlen(%{t:Ox.mem})); |} : int)] in
    let get i =
      [%c.alloc
        ({|
           CAMLreturn (Val_int(((const char* )%{t:Ox.mem})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.mem (no_alloc)" =
  let is_null (t : Ox.mem) = t |> Ox.Mem.to_ptr |> Ox.Ptr.Ext.is_null in
  let t = [%c.no_alloc ({| return (NULL); |} : Ox.mem)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.no_alloc ({| return ("Hello C!"); |} : Ox.mem)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.no_alloc ({| return (strlen(%{t:Ox.mem})); |} : int)] in
    let get i =
      [%c.no_alloc
        ({|
           return (Val_int(((const char* )%{t:Ox.mem})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Ptr.Ext.t (alloc)" =
  let is_null (t : Ox.u8 Ox.Ptr.Ext.t) = t |> Fn.id |> Ox.Ptr.Ext.is_null in
  let t = [%c.alloc ({| CAMLreturn (NULL); |} : Ox.u8 Ox.Ptr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.alloc ({| CAMLreturn ("Hello C!"); |} : Ox.u8 Ox.Ptr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.alloc ({| CAMLreturn (strlen(%{t:Ox.u8 Ox.Ptr.Ext.t})); |} : int)] in
    let get i =
      [%c.alloc
        ({|
           CAMLreturn (Val_int(((const char* )%{t:Ox.u8 Ox.Ptr.Ext.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Ptr.Ext.t (no_alloc)" =
  let is_null (t : Ox.u8 Ox.Ptr.Ext.t) = t |> Fn.id |> Ox.Ptr.Ext.is_null in
  let t = [%c.no_alloc ({| return (NULL); |} : Ox.u8 Ox.Ptr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.no_alloc ({| return ("Hello C!"); |} : Ox.u8 Ox.Ptr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.no_alloc ({| return (strlen(%{t:Ox.u8 Ox.Ptr.Ext.t})); |} : int)] in
    let get i =
      [%c.no_alloc
        ({|
           return (Val_int(((const char* )%{t:Ox.u8 Ox.Ptr.Ext.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Ptr.Ext.Imm.t (alloc)" =
  let is_null (t : Ox.u8 Ox.Ptr.Ext.Imm.t) =
    t |> Ox.Ptr.Ext.of_imm |> Ox.Ptr.Ext.is_null
  in
  let t = [%c.alloc ({| CAMLreturn (NULL); |} : Ox.u8 Ox.Ptr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.alloc ({| CAMLreturn ("Hello C!"); |} : Ox.u8 Ox.Ptr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len =
      [%c.alloc ({| CAMLreturn (strlen(%{t:Ox.u8 Ox.Ptr.Ext.Imm.t})); |} : int)]
    in
    let get i =
      [%c.alloc
        ({|
           CAMLreturn (Val_int(((const char* )%{t:Ox.u8 Ox.Ptr.Ext.Imm.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Ptr.Ext.Imm.t (no_alloc)" =
  let is_null (t : Ox.u8 Ox.Ptr.Ext.Imm.t) =
    t |> Ox.Ptr.Ext.of_imm |> Ox.Ptr.Ext.is_null
  in
  let t = [%c.no_alloc ({| return (NULL); |} : Ox.u8 Ox.Ptr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.no_alloc ({| return ("Hello C!"); |} : Ox.u8 Ox.Ptr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.no_alloc ({| return (strlen(%{t:Ox.u8 Ox.Ptr.Ext.Imm.t})); |} : int)] in
    let get i =
      [%c.no_alloc
        ({|
           return (Val_int(((const char* )%{t:Ox.u8 Ox.Ptr.Ext.Imm.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Addr.Ext.t (alloc)" =
  let is_null (t : Ox.u8 Ox.Addr.Ext.t) = t |> Ox.Addr.Ext.to_ptr |> Ox.Ptr.Ext.is_null in
  let t = [%c.alloc ({| CAMLreturn (NULL); |} : Ox.u8 Ox.Addr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.alloc ({| CAMLreturn ("Hello C!"); |} : Ox.u8 Ox.Addr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.alloc ({| CAMLreturn (strlen(%{t:Ox.u8 Ox.Addr.Ext.t})); |} : int)] in
    let get i =
      [%c.alloc
        ({|
           CAMLreturn (Val_int(((const char* )%{t:Ox.u8 Ox.Addr.Ext.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Addr.Ext.t (no_alloc)" =
  let is_null (t : Ox.u8 Ox.Addr.Ext.t) = t |> Ox.Addr.Ext.to_ptr |> Ox.Ptr.Ext.is_null in
  let t = [%c.no_alloc ({| return (NULL); |} : Ox.u8 Ox.Addr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.no_alloc ({| return ("Hello C!"); |} : Ox.u8 Ox.Addr.Ext.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len = [%c.no_alloc ({| return (strlen(%{t:Ox.u8 Ox.Addr.Ext.t})); |} : int)] in
    let get i =
      [%c.no_alloc
        ({|
           return (Val_int(((const char* )%{t:Ox.u8 Ox.Addr.Ext.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Addr.Ext.Imm.t (alloc)" =
  let is_null (t : Ox.u8 Ox.Addr.Ext.Imm.t) =
    t |> Ox.Addr.Ext.Imm.to_ptr |> Ox.Ptr.Ext.of_imm |> Ox.Ptr.Ext.is_null
  in
  let t = [%c.alloc ({| CAMLreturn (NULL); |} : Ox.u8 Ox.Addr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.alloc ({| CAMLreturn ("Hello C!"); |} : Ox.u8 Ox.Addr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len =
      [%c.alloc ({| CAMLreturn (strlen(%{t:Ox.u8 Ox.Addr.Ext.Imm.t})); |} : int)]
    in
    let get i =
      [%c.alloc
        ({|
           CAMLreturn (Val_int(((const char* )%{t:Ox.u8 Ox.Addr.Ext.Imm.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

let%expect_test "Ox.Addr.Ext.Imm.t (no_alloc)" =
  let is_null (t : Ox.u8 Ox.Addr.Ext.Imm.t) =
    t |> Ox.Addr.Ext.Imm.to_ptr |> Ox.Ptr.Ext.of_imm |> Ox.Ptr.Ext.is_null
  in
  let t = [%c.no_alloc ({| return (NULL); |} : Ox.u8 Ox.Addr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null true) |}];
  let t = [%c.no_alloc ({| return ("Hello C!"); |} : Ox.u8 Ox.Addr.Ext.Imm.t)] in
  print_s [%message "" ~is_null:(is_null t : bool)];
  [%expect {| (is_null false) |}];
  let to_string t =
    let len =
      [%c.no_alloc ({| return (strlen(%{t:Ox.u8 Ox.Addr.Ext.Imm.t})); |} : int)]
    in
    let get i =
      [%c.no_alloc
        ({|
           return (Val_int(((const char* )%{t:Ox.u8 Ox.Addr.Ext.Imm.t})[%{i:int}])); 
         |}
         : char value)]
    in
    String.init len ~f:get
  in
  print_endline (to_string t);
  [%expect {| Hello C! |}]
;;

(*$*)
