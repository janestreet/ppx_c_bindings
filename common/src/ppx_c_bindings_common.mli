open Core
open Ppxlib

val c_generator_command : unit -> Command.t
val ml_generator_impl : structure -> structure

module For_testing : sig
  module Type_ = Type_.For_testing
end
