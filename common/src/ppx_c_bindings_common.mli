open Core
open Ppxlib

val c_generator_command : unit -> Command.t
val ml_generator_impl : structure -> structure
