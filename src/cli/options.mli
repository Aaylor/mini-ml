
module type StorageParameter = sig
  type t
  val set : t -> unit
  val get : unit -> t
end

module type BooleanParameter = StorageParameter with type t := bool
module type StringParameter  = StorageParameter with type t := string

module PrintProgram : BooleanParameter

module CleanTypes : BooleanParameter

module File : StringParameter

module Unsafe : BooleanParameter

module Interpreter : BooleanParameter

module Interactive : BooleanParameter
