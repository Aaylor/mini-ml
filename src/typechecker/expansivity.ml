open Caml

let is_binary_op_except_ref = function
  | "+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">=" | "="
  | "<>" | "!" | ":=" -> true
  | _ -> false

let is_op_except_ref = function
  | Var x when is_binary_op_except_ref x -> true
  | _ -> false

let rec is_expansive = function
  | Let ({ expr }, e2) -> is_expansive expr || is_expansive e2
  | Pair (e1, e2)
  | Cons (e1, e2) -> is_expansive e1 || is_expansive e2 
  | App (x, e2) when is_op_except_ref x && not (is_expansive e2) -> false
  | App (_, _) -> true
  | Var  _
  | Bool _
  | Int  _
  | Fun  _
  | Nil -> false
