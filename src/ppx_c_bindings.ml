let () =
  Ppxlib.Driver.register_transformation
    "ppx_c_bindings"
    ~impl:Ppx_c_bindings_common.ml_generator_impl
;;
