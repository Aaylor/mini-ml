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

let rec string_of_expression_type = function
  | Variable v -> string_of_var v
  | BaseType b -> string_of_base_type b
  | Arrow (t1, t2) ->
    Printf.sprintf "%s -> %s"
      (string_of_expression_type t1) (string_of_expression_type t2)
  | Star (t1, t2) ->
    Printf.sprintf "(%s * %s)"
      (string_of_expression_type t1) (string_of_expression_type t2)
  | List t ->
    Printf.sprintf "%s list" (string_of_expression_type t)
  | Ref t ->
    Printf.sprintf "%s ref" (string_of_expression_type t)
  | Unit ->
    "unit"

and string_of_base_type = function
  | Int -> "int"
  | Bool -> "bool"

and string_of_var {name; content} =
  let ty = match content with
    | None -> if (Options.CleanTypes.get ()) then name else "none"
    | Some ty -> string_of_expression_type ty
  in
  if (Options.CleanTypes.get ()) then ty
  else Printf.sprintf "(%s : %s)" name ty
