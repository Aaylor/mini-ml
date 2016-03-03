
module type StorageParameter = sig
  type t
  val set : t -> unit
  val get : unit -> t
end

module type InitialParameter = sig
  type t
  val init : t
end

module type BooleanParameter = StorageParameter with type t := bool

module type StringParameter  = StorageParameter with type t := string



module StorageParameterImpl (P : InitialParameter) = struct
  type t = P.t
  let value = ref P.init
  let set t = value := t
  let get () = !value
end

module BooleanParam = struct
  type t = bool
end
module False = struct include BooleanParam let init = false end
module True  = struct include BooleanParam let init = true  end

module StringParam = struct
  type t = string
  let init = ""
end

module PrintProgram = StorageParameterImpl(False)

module CleanTypes = StorageParameterImpl(False)

module File = StorageParameterImpl(struct type t = string let init = "" end)

module Unsafe = StorageParameterImpl(False)

module Interpreter = StorageParameterImpl(True)

module Interactive = StorageParameterImpl(False)
