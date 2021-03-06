open Types

exception Unbound of string

include Map.Make(struct
    type t = string
    let compare = String.compare
  end)

let find key map =
  try find key map with Not_found -> raise (Unbound key)

let string_of_env env f =
  fold (fun k v acc ->
      Printf.sprintf "%s%s : %s\n" acc k (f v)
    ) env ""
