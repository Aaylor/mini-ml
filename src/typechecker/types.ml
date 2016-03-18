type expression_type =
  | Variable of var
  | BaseType of base_type
  | Arrow of expression_type * expression_type
  | Star  of expression_type * expression_type
  | List  of expression_type
  | Ref   of expression_type
  | Unit

and base_type =
  | Int
  | Bool

and var = {
  name : string;
  mutable content : expression_type option
}


(* Variables *)

module VarSet = struct
  include Set.Make(struct
      type t = string
      let compare = String.compare
    end)

  let to_list s = fold (fun x a -> x :: a) s []
end

(* Equality functions *)

let rec et_eq e1 e2 =
  match e1, e2 with
  | Variable v1, Variable v2 -> var_eq v1 v2
  | BaseType bt1, BaseType bt2 -> bt_eq bt1 bt2
  | Arrow(e1, e2), Arrow(e3, e4)
  | Star(e1, e2), Star(e3, e4) -> et_eq e1 e3 && et_eq e2 e4
  | _ -> false

and bt_eq e1 e2 =
  e1 = e2

and var_eq v1 v2 =
  v1.name = v2.name || match v1.content, v2.content with
  | None, None       -> false
  | Some t1, Some t2 -> et_eq t1 t2
  | None, Some t2    -> et_eq (Variable v1) t2
  | Some t1, None    -> et_eq t1 (Variable v2)

and var_eq_name v1 v2 =
  v1.name = v2.name



(* Printer *)

module VarMap = struct
  include Map.Make(struct
      type t = string
      let compare = String.compare
    end)
end

let range x y =
  let rec aux acc x y =
    if x > y then acc else aux (y :: acc) x (y - 1)
  in
  aux [] x y

let letters = List.map Char.chr (range (Char.code 'a') (Char.code 'z'))

let make_generator () =
  let stream = Stream.of_list letters in
  fun () -> Stream.next stream

let replace env gen var =
  try
    let elt = VarMap.find var env in
    (elt, env)
  with Not_found ->
    let new_var = Printf.sprintf "'%c" (gen ()) in
    let env' = VarMap.add var new_var env in
    (new_var, env')




let rec string_of_expression_type ty =
  fst (pretty_string_of_expression_type (make_generator ()) VarMap.empty ty)

and pretty_string_of_expression_type gen env = function
  | Variable v -> string_of_var gen env v
  | BaseType b -> string_of_base_type b, env
  | Arrow (t1, t2) ->
    let str1, env' = pretty_string_of_expression_type gen env  t1 in
    let str2, env' = pretty_string_of_expression_type gen env' t2 in
    Printf.sprintf "(%s -> %s)" str1 str2, env'
  | Star (t1, t2) ->
    let str1, env' = pretty_string_of_expression_type gen env  t1 in
    let str2, env' = pretty_string_of_expression_type gen env' t2 in
    Printf.sprintf "(%s * %s)" str1 str2, env'
  | List t ->
    let str, env' = pretty_string_of_expression_type gen env t in
    Printf.sprintf "%s list" str, env'
  | Ref t ->
    let str, env' = pretty_string_of_expression_type gen env t in
    Printf.sprintf "%s ref" str, env'
  | Unit ->
    "unit", env

and string_of_base_type = function
  | Int -> "int"
  | Bool -> "bool"

and string_of_var gen env {name; content} =
  let ty = match content with
    | None ->
      if (Options.CleanTypes.get ()) then
        replace env gen name
      else
        ("none", env)
    | Some ty ->
      pretty_string_of_expression_type gen env ty
  in
  if (Options.CleanTypes.get ()) then ty
  else Printf.sprintf "(%s : %s)" name (fst ty), (snd ty)
