open! Core
open! Ppxlib
open! Import

let c_generator_command = C_code_gen.command
let ml_generator_impl = Ocaml_code_gen.impl

module For_testing = struct
  module Type_ = Type_.For_testing
end
