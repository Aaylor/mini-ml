

module Memory : sig

  type address
  type 'a memory  

  exception Out_of_memory
  
  val init_memory : 'a -> 'a memory
  
  val store : 'a -> 'a memory -> address

  val update : address -> 'a -> 'a memory -> unit

  val get : address -> 'a memory -> 'a

  val string_of_address : address -> string
  
end = struct

  type address = int
  type 'a memory = 'a array

  exception Out_of_memory
  
  let max_size = 32456

  let init_memory elt =
    Array.make max_size elt

  let addr = ref (-1)
  
  let next_address =
    fun () ->
      incr addr;
      if !addr = max_size then raise Out_of_memory else !addr

  let store value memory =
    let addr = next_address () in
    memory.(addr) <- value;
    addr

  let update address value memory =
    if address <= !addr then memory.(address) <- value
    else failwith "update: Index Out Of Bound"
    
  let get address memory =
    if address <= !addr then memory.(address)
    else failwith "get: Index Out Of Bound"
  
  let string_of_address = string_of_int

end

