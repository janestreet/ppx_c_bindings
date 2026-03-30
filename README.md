ppx_c_bindings
==============



This ppx allows you to interface with C libraries by writing C code inline in your OCaml
files. It's much easier and more flexible than the traditional way of writing C bindings,
and should improve code quality and development velocity.

Examples
--------
  - [`OCaml-torch`](https://github.com/janestreet/torch)
  - [`ppx_c_bindings/test`](https://github.com/janestreet/ppx_c_bindings/tree/master/test)


Summary
=========

`ppx_c_bindings` generates a C file with OCaml bindings from your OCaml file, and calls
those bindings from OCaml where appropriate.

You can do just about anything you can do in a C file:

  - write arbitrary functions
  - call C functions and get their return values
  - define structs that correspond to OCaml types
  - allocate OCaml values
  - manipulate the runtime lock

One limitation is that you can't currently use this to write foreign bindings that work
with both native and `js_of_ocaml` compilation.


Getting Started
===============
The `jbuild` Setup
------------------
To add C bindings in `${NAME}.ml`, add the following
to the `library` or `executables` stanza in your `jbuild`
```jbuild
  (c_names (${NAME}_stubs))
  (preprocess (pps (ppx_jane ppx_c_bindings)))
```
and then add the following to the end of the `jbuild`
```jbuild
;; generate the supporting c code
(rule (
  (deps    (${NAME}.ml))
  (targets (${NAME}_stubs.c))
  (action "%{bin:ppx-c-bindings} %{deps}>%{target}")))

;; disable formating of the generated files
(enforce_style ((exceptions (${NAME}_stubs.c))))
```
You can include multiple files by adding them to the `c_names` and `enforce_style` `exceptions` lists,
and repeating the generation `rule` for each file.

Ignoring generated C code
-------------------------
Ignore the generated C files from being commited by adding the filenames to the `.hgignore.in` of each directory where the files are generated.

```.hgignore.in
${NAME}_stubs.c
```

Syntax
======
Top level C
-----------

You can use the `[%%c]` directive to copy code directly into the generated C file. This is
useful for header includes and helper functions.

```ocaml
[%%c {|
#include <math.h>
#include <stdio.h>
|}]
```

```ocaml
[%%c {|
int add_one(int x) {
  return x + 1;
}
|}]
```
This will be copied verbatim into the generated C code.
Use it to include headers or add defines or helper functions in C.

Expressions
-----------
You can include individual C expressions using the `[%c.no_alloc]` or `[%c.alloc]` directives
(most of the time you probably want `[%.no_alloc]` but see (#when_to_use_alloc) for more details).

```ocaml
(* Prints "Hello World!" *)
[%c.no_alloc {| printf(stdout, "Hello World!"); |}]
```

### Passing parameters

You can also pass arguments to the C expression:
```ocaml
(* Prints "The answer is 42!" *)
let answer = 42 in
[%c.no_alloc {| printf("The answer is %ld!", %{answer:int}); |}]

```
We have special handling for various primitive types:

| OCaml Type     | C Type      | Syntax                      |
| -------------- | ----------- | --------------------------- |
| `bool`         | `bool`      | `%{IDENT:bool}`             |
| `int`          | `nativeint` | `%{IDENT:int}`              |
| `Int32.t`      | `int32_t`   | `%{IDENT:Int32.t}`          |
| `Int64.t`      | `int64_t`   | `%{IDENT:Int64.t}`          |
| `float`        | `double`    | `%{IDENT:float}`            |
| `TYPE`         | `value`     | `%{IDENT:TYPE value}`       |
| `local_ TYPE`  | `value`     | `%{IDENT:TYPE local_value}` |

You can use other types with the syntax `%{IDENT:TYPE value}` (see the [Types](#types)
section for details on how to use custom types). You can use the `TYPE_val` macro in C to
convert the value to the appropriate pointer type ( see [OCaml manual section
4.3](https://v2.ocaml.org/manual/intfc.html#ss:c-block-access) for details). For example,
you can read a string using the `String_val` macro:

```ocaml
(* Prints "Hello Tias!" *)
let name = "Tias" in
[%c.no_alloc {| printf("Hello %s!", String_val(%{name:string value})); |}]
```

If you use the same value multiple times, you only need to include the type on the first
occurrence.

```ocaml
(* Prints "buffalo buffalo buffalo buffalo buffalo" *)
let word = "buffalo" in
[%c.no_alloc {| printf("%s %s %s %s %s.", String_val(%{word:string value}), String_val(%{word}), String_val(%{word}), String_val(%{word}),  String_val(%{word})); |}]
```

<div class="note" style="margin:10px 10px; padding:0px 10px;padding-bottom:10px; font-weight:normal;">
If you've worked with C bindings before you know that you normally need to list all your
arguments using `CAMLparam`, `ppx_c_bindings` will automatically do this for you.
</div>

### Returning values

By default we assume your expression will not give a result (`void` in C, `unit` in
ocaml). To return a **primitive** value from the C expression to your OCaml code, you
can annotate your function call with the appropriate type, e.g.:
```ocaml
let result = [%c.no_alloc ({|return sin(%{theta:float});|} : float)]
```

<div class="alert" style="margin:10px 10px; padding:0px 10px;padding-bottom:10px; font-weight:normal;">
To return a more complex type you generally need to use `[%c.alloc]`.
</div>

### When should I use `[%c.no_alloc]` vs `[%c.alloc]`
Generally the recommendation is to write your bindings with simple C snippets that
do not need to do any allocation (on the ocaml heap), but sometimes there are exceptions.


In general if you do anything that requires interaction with the OCaml GC:

- allocation
- releasting the runtime lock
- [raising exceptions](https://ocaml.org/manual/5.2/intfc.html#ss:c-exceptions)
- [calling back into OCaml code](https://ocaml.org/manual/5.2/intfc.html#s:c-callback)

then you need to use `[%c.alloc]`.

#### Allocating
Sometimes its helpful to return more than just primitive values from C.

```ocaml
(* Returns the string "hello" as an OCaml string *)
let one = [%c.alloc ({| CAMLreturn(caml_copy_string("hello")); |}:string value) ]
```

If you want to return anything other than an int,float or boolean you need to:

- wrap the value in a form the OCaml runtime expects
 ([see the OCaml manual chapter on simple allocations](https://ocaml.org/manual/5.2/intfc.html#sss:c-simple-allocation))
- use the appropriate type annotation, then `TYPE value` or `local TYPE value` is
  important as it indicates that you are providing the correct runtime representation.
- use `[%c.alloc]` since you are allocating an OCaml value
- use [`CAMLreturn`](https://v2.ocaml.org/manual/5.2/intfc.html#ss:c-simple-gc-harmony)
  (and possibly also `CAMLlocal`).

although if you're just copying a string the example above is all you need.

<div class="alert" style="margin:10px 10px; padding:0px 10px;padding-bottom:10px; font-weight:normal;">
If you are doing any allocation beyond copying a string as in the example you should
read the section on GC Harmony and allocations.
</div>

#### The Runtime Lock

There are lots of really useful C libraries where the functionality you want is
expensive in terms of CPU or wall time. If you're using async you probably don't want to
lock up your program while these functions run.

This is where the OCaml runtime lock comes in.

```ocaml
(* Sleep for 30 seconds *)
In_thread.run (fun () ->
[%c.alloc {|
  caml_release_runtime_system();
  sleep(30);
  caml_acquire_runtime_system();
|}])
```

You probably want to review the OCaml manual chapter [multithreading in c bindings](https://ocaml.org/manual/5.2/intfc.html#s:C-multithreading), however the general rules are:

- use `[%c.alloc` (and if returning a value  `CAMLreturn`)
- You must call `caml_release_runtime_system()`, followed by  `caml_acquire_runtime_system()`
- You may not use `%{.... : ... value}` anywhere between the release and acquire.
- You may not call any other OCaml runtime functions (other then what is described in the
  manual), so no allocating or raising exceptions.


Types
-----
You can't always just use primitive types, sometimes the library you're wrapping defines
its own types that you need to pass around.

```ocaml
[%%c {|
struct my_c_struct {
  nativeint[2] fields;
  uint8_t *ptr;
}
|}]
type foo = [%c {| struct my_c_struct |}
           ~free:{| if (t->ptr != NULL) {
                      free(t->ptr);
                      t->ptr = NULL;
                    } |}]
```
This will define:

- an OCaml abstract type `foo` that contains the data specified in the C struct
  definition. When the OCaml runtime garbage collects a value of this type, it will call
  [free] with `t` as a pointer to the structure.

- a C macro `Foo_val(...)` that you can use to access the content of `foo` within C code.

- a function `val alloc_foo : unit -> foo` that you can use to create a new value of type
  `foo` in OCaml. The value will be uninitialized (not necessarily zero-initialized), but
  you can use [%c.no_alloc] (or [%c.alloc]) and `Foo_val` to populate it.

The content of the type will be on the OCaml heap, so you may only access it if you have
the runtime lock, and the value may be moved around during garbage collection.

Most C libraries do not handle this well, so typically you will just have a pointer to the
actual C data structure allocated outside the OCaml heap.

```ocaml
type c_string = [%c {| char* |}
             ~free:{| if ( *t != NULL ) { free( *t ); } |}]

let init () =
  let t = alloc_c_string () in

  (* [no_alloc] is Ok here since [*v] gets allocated on outside of the OCaml heap by `calloc` *)
  let () = [%c.no_alloc {|
    char *v;
    v = calloc(100);
    *C_string_val(%{t:t value}) = v;
  |}] in
  t
```

How It Works
============

This PPX works in two passes.
The first pass runs as a normal OCaml PPX. It discards top level `[%%c ...]` statements and rewrites `[%c ...]` expressions to external calls as follows.
```ocaml
[%c.no_alloc ({|return sin(%{theta:float});|} : float)],
[%c.alloc ({|CAMLreturn(caml_copy_string("Hi!");|} : string value)]
```
... turns into ...
```ocaml
(let open struct
  external unique_name : float[@unboxed] -> float[@unboxed]=
                           "FILE_long_unique_name_bytecode" "FILE_long_unique_name_native"
end in unique_name theta),
(let open struct
  external unique_name2 : unit -> string =
                            "FILE_long_unique_name2_bytecode" "FILE_long_unique_name2_native"
end in unique_name2 ()),
```
The second pass (the `rule` in the jbuild above), will generate a C file, by copying top level expressions, and generating suitable functions to enclose the code.
```c
CAMLprim double FILE_long_unique_name_native(double theta) {
  return sin((theta));
}
CAMLprim value FILE_long_unique_name_bytecode(value theta) {
  return caml_alloc_double(FILE_long_unique_name_native(Double_val(theta)))
}
CAMLprim value FILE_long_unique_name2_native() {
  CAMLparam0();
  CAMLreturn(caml_copy_string("Hi!"));
}
CAMLprim value FILE_long_unique_name2_bytecode() {
  return FILE_long_unique_name_native2();
}
```

Resources
=========

C bindings can be tricky. The following may be helpful to get a better understanding
of the details.

- [Real World OCaml: Foreign Function Interface](https://dev.realworldocaml.org/foreign-function-interface.html)
- [The OCaml Manual: Interfacing C with OCaml](https://v2.ocaml.org/manual/intfc.html)
- [Easy Mistakes when Writing OCaml C Bindings](https://www.brendanlong.com/easy-mistakes-when-writing-ocaml-c-bindings.html)
