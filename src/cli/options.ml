
let __clean_types = ref false
let set_clean_types t = __clean_types := t
let clean_types () = !__clean_types

let __file : string option ref = ref None
let set_file s = __file := Some s
let file () = match !__file with
  | None -> failwith "File is not set..."
  | Some s -> s

let __unsafe = ref false
let set_unsafe t = __unsafe := t
let unsafe () = !__unsafe
