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

C++ bindings are also supported via a dedicated mode; see the [C++ Bindings](#c-bindings)
section below.


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

| OCaml Type     | Parameter Syntax            | C Type      |
| -------------- | --------------------------- | ----------- |
| `int`          | `%{IDENT:int}`              | `nativeint` |
| `Int32.t`      | `%{IDENT:Int32.t}`          | `int32_t`   |
| `Int64.t`      | `%{IDENT:Int64.t}`          | `int64_t`   |
| `float`        | `%{IDENT:float}`            | `double`    |
| `TYPE`         | `%{IDENT:TYPE value}`       | `value`     |
| `local_ TYPE`  | `%{IDENT:TYPE local_value}` | `value`     |


You can use other types with the syntax `%{IDENT:TYPE value}` (see the [Types](#types)
section for details on how to use custom types). You can use the `TYPE_val` macro in C to
convert the value to the appropriate pointer type ( see [OCaml manual section
4.3](https://v2.ocaml.org/manual/intfc.html#ss:c-block-access) for details). For example,
you can read a string using the `String_val` macro:

```ocaml
(* Prints "Hello Streeter!" *)
let name = "Streeter" in
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


Similar to parameters we have special handling that you can use for many cases.

| OCaml Type      | Return Syntax      | C Type                  |
| --------------  | ------------------ | -----------             |
| `unit`          | none               | implemented as Val_unit |
| `int`           | `int`              | `nativeint`             |
| `Int32.t`       | `Int32.t`          | `int32_t`               |
| `Int64.t`       | `Int64.t`          | `int64_t`               |
| `float`         | `float`            | `double`                |
| `TYPE`          | `TYPE value`       | `value`                 |
| `local_ TYPE`   | `TYPE local_value` | `value`                 |


### Unboxed Types (OxCaml only)

When you are building with oxcaml we additionally allow the following parameter
and return types.

| OCaml Type / Return Syntax | Parameter Syntax                | C Type        |
| -------------------------- | ------------------------------- | ------------- |
| `Ox.u8`                    | `%{IDENT:Ox.u8}`                | `uint8_t`     |
| `Ox.i8`                    | `%{IDENT:Ox.i8}`                | `int8_t`      |
| `Ox.u16`                   | `%{IDENT:Ox.u16}`               | `uint16_t`    |
| `Ox.i16`                   | `%{IDENT:Ox.i16}`               | `int16_t`     |
| `Ox.u32`                   | `%{IDENT:Ox.u32}`               | `uint32_t`    |
| `Ox.i32`                   | `%{IDENT:Ox.i32}`               | `int32_t`     |
| `Ox.u64`                   | `%{IDENT:Ox.u64}`               | `uint64_t`    |
| `Ox.i64`                   | `%{IDENT:Ox.i64}`               | `int64_t`     |
| `Ox.f32`                   | `%{IDENT:Ox.f32}`               | `float`       |
| `Ox.f64`                   | `%{IDENT:Ox.f64}`               | `double`      |
| `Ox.isize`                 | `%{IDENT:Ox.isize}`             | `isize_t`     |
| `Ox.mem`                   | `%{IDENT:Ox.mem}`               | `void*`       |
| `'a Ox.Ptr.Ext.t`          | `%{IDENT:'a Ox.Ptr.Ext.t}`      | `void*`       |
| `'a Ox.Ptr.Ext.Imm.t`      | `%{IDENT:'a Ox.Ptr.Ext.Imm.t}`  | `const void*` |
| `'a Ox.Addr.Ext.t`         | `%{IDENT:'a Ox.Addr.Ext.t}`     | `void*`       |
| `'a Ox.Addr.Ext.Imm.t`     | `%{IDENT:'a Ox.Addr.Ext.Imm.t}` | `const void*` |

FYI: We redefine `CAMLreturn` where appropriate to have the correct type, so you
do not need to use `CAMLreturnT` explicitly.

#### Restrictions

- This is only supported when using the OxCaml compiler as this currently relies
  on features that have not yet landed in the upstream OCaml compiler.


### Returning unboxed tuples (OxCaml only)

You can return a 2-element unboxed tuple from `[c...]` expressions.
The native C function returns a small struct via the SysV small-struct
ABI (in `rax`/`rdx` on x86-64).

Within the C body you populate the result using the compound literal syntax.

```ocaml
let #(a, b) = [%c.no_alloc ({| return { 42, Val_int(24) }; |} : #(Ox.i64 * int value))]
```

or

```ocaml
let #(a, b) = [%c.no_alloc ({| CAMLreturn( { 42, Val_int(24) } ); |} : #(Ox.i64 * int value))]
```

The struct field types follow each component's natural OxCaml layout:

| Component         | C field type      |
| ----------------- | ----------------- |
| `Ox.*`            | See Unboxed Types |
| `T value`         | `value`           |
| `T local_value`   | `value`           |


##### Restrictions:

- Returning unboxed products from externals is OxCaml-only.
- Native compilation only 
- Only allowed in the return position (no unboxed tuple arguments).
- Only up to two element tuples (this is from the OxCaml/C ABI and enforced/checked by the compiler)
- No nested unboxed tuples (unsupported by the compiler)
- The types `int`, `float`, `Int32.t` and `Int64.t` are not currently supported in tuples
  (Bad interaction with `[@untagged]`/`[@unboxed]` attributes), use an appropriate `Ox.`
  type instead.

### When should I use `[%c.no_alloc]` vs `[%c.alloc]`
Generally the recommendation is to write your bindings with simple C snippets that
do not need to do any allocation (on the ocaml heap), but sometimes there are exceptions.


In general if you do anything that requires interaction with the OCaml GC:

- allocation
- releasting the runtime lock
- [raising exceptions](https://ocaml.org/manual/5.2/intfc.html#ss:c-exceptions)
- [calling back into OCaml code](https://ocaml.org/manual/5.2/intfc.html#s:c-callback)

then you need to use `[%c.alloc]`.

Tip: you can use go-lang style tuple returns (see [unboxed tuples](#Returning-unboxed-tuples) above) to
return errors without requiring allocations.

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

- a C macro `Foo_alloc()` that you can use to construct a value of this type from C.

- a function `val alloc_foo : unit -> foo` that you can use to create a new value of type
  `foo` in OCaml. The value will be uninitialized (not necessarily zero-initialized), but
  you can use [%c.no_alloc] (or [%c.alloc]) and `Foo_val` to populate it.

- If the type does not define a ~free (GC finalizers are not supported in stack local
  allocations), then additionally you get
    - a C macro `Foo_alloc__stack()` to construct a stack local value of the type
    - and `val alloc_foo__stack : unit -> foo @ local` (best called as `(alloc_foo[@alloc stack])`) to do the same in OCaml.

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
                           "PPX_C_BINDINGS_DOES_NOT_SUPPORT_BYTE_CODE" "FILE_long_unique_name"
end in unique_name theta),
(let open struct
  external unique_name2 : unit -> string =
                            "PPX_C_BINDINGS_DOES_NOT_SUPPORT_BYTE_CODE" "FILE_long_unique_name2"
end in unique_name2 ()),
```
The second pass (the `rule` in the jbuild above), will generate a C file, by copying top level expressions, and generating suitable functions to enclose the code.
```c
CAMLprim double FILE_long_unique_name(double theta) {
  return sin((theta));
}
CAMLprim value FILE_long_unique_name2() {
  CAMLparam0();
  CAMLreturn(caml_copy_string("Hi!"));
}
```

- `ppx_c_bindings` ppx expansion and generated C code are only supported when used with
  a native target of ocamlc and are not compatible with the bytecode compiler.
  You should get build time linker errors if this applies to you.
  (Earlier versions of `ppx_c_bindings` included some simple bytecode bindings but these
  were unused so far as we know and entirely untested).


C++ Bindings
============

You can use `ppx_c_bindings` to bind to C++ libraries. Pass `-cpp to the code generator
and target a `.cpp` output file. In C++ mode, each generated/inserted C snippet (type
definitions, expression wrappers, and the contents of `[%%c ...]` blocks) is
individually wrapped in its own `extern "C" { ... }` block, so OCaml sees ordinary
C-linkage symbols. `[%%cpp ...]` blocks are emitted verbatim (not wrapped).

### `jbuild` setup

Use `cxx_names` / `cxx_flags` and generate a `.cpp` file:

```jbuild
  (cxx_names (${NAME}_stubs))
  (cxx_flags (:standard -std=gnu++17))
  (preprocess (pps (ppx_jane ppx_c_bindings)))
```

C++ mode is selected by the stub-generation rule: pass `-cpp` to `ppx-c-bindings` and
emit a `.cpp` output. The PPX itself always accepts `[%%cpp ...]` (it is a no-op at the
OCaml level), so there is no separate PPX flag to keep in sync.

```jbuild
(rule (
  (deps    (${NAME}.ml))
  (targets (${NAME}_stubs.cpp))
  (action "%{bin:ppx-c-bindings} -cpp %{deps}>%{target}")))
```

If `[%%cpp ...]` is used without `-cpp` on `ppx-c-bindings`, the stub generator emits a
`#error` directive at the original source location, so the mismatch is reported by the
C++ compiler instead of silently producing broken stubs.

### Writing C++ only snippets with `[%%cpp ...]`

`[%%c ...]` blocks land inside the generated `extern "C" { ... }` wrapper. That is fine
for C headers (e.g. `<math.h>`, `<string.h>`), which typically have their own `extern "C"`
guards internally, but it does **not** work for C++ headers such as `<string>` or
`<vector>`, which must be included at C++ linkage.

Use `[%%cpp ...]` for anything that needs C++ linkage: C++ standard library includes,
`namespace` declarations, helper classes / templates, etc.

```ocaml
[%%cpp
  {|
#include <string>
#include <vector>

namespace my_helpers {
  static std::string greet(const std::string& who) {
    return std::string("hello, ") + who;
  }
}
|}]
```

You can freely mix `[%%c ...]` and `[%%cpp ...]` blocks. Inside `[%c.no_alloc]` /
`[%c.alloc]` expression bodies you can still use C++ features such as STL types and
lambdas, because the body itself is compiled as C++; only the function's *linkage* is
`"C"` (the body can use any C++ construct, subject to the caveats below).

### Restrictions on `extern "C"` function bodies

`extern "C"` only changes *linkage* (no name mangling, no overloading); it does not
restrict what C++ features can appear *inside* the function body. In practice,
effectively anything that is legal in a normal C++ function body is legal in the bodies
`ppx_c_bindings` generates for you:

  - local variables of C++ class types (including STL types such as `std::string`,
    `std::vector`, etc.)
  - RAII, destructors, lambdas, `auto`, range-`for`, templated helpers, etc.
  - `throw` and `try` / `catch`, as long as exceptions are caught *before* returning

The useful restrictions to keep in mind are:

  - **Exceptions must not propagate back to the OCaml runtime.** The generated entry
    points have C linkage and are called directly from OCaml; if a C++ exception leaks
    out of a binding, behavior is implementation-defined (typically `std::terminate`).
    Wrap fallible C++ code in `try { ... } catch (...) { ... }` and convert failures into
    an OCaml-visible error (e.g. raise an OCaml exception via `caml_failwith` /
    `caml_raise_*`).
  - **Function overloading / default arguments / member functions don't apply**: the
    generated functions are top-level C-linkage functions with unique names, so these
    purely-declaration-level C++ features aren't relevant.

If support for features like "automatically convert C++ exceptions to OCaml exceptions"
would be useful, it is a natural next extension to this mode.

Resources
=========

C bindings can be tricky. The following may be helpful to get a better understanding
of the details.

- [Real World OCaml: Foreign Function Interface](https://dev.realworldocaml.org/foreign-function-interface.html)
- [The OCaml Manual: Interfacing C with OCaml](https://v2.ocaml.org/manual/intfc.html)
- [Easy Mistakes when Writing OCaml C Bindings](https://www.brendanlong.com/easy-mistakes-when-writing-ocaml-c-bindings.html)
