
open Caml

let pri_o2 = function
  | "!" -> 12
  | "*" | "/" -> 10
  | "+" | "-" -> 6
  | "=" | "<>" | "<" | ">" | "<=" | ">=" -> 4
  | "&&" -> 3
  | "||" -> 2
  | ":=" -> 1
  | _ -> 0

let pri_fun = 1
let pri_let = 1
let pri_cons = 5
let pri_app = 12

let paren_if b s = if b then "("^s^")" else s

let pr_bool = function true -> "true" | false -> "false"

let rec pr_motif in_cons = function
  | MVar v -> v
  | MBool b -> pr_bool b
  | MInt n -> string_of_int n
  | MPair (m1,m2) -> "("^pr_motif false m1^","^pr_motif false m2^")"
  | MNil -> "[]"
  | MCons (m1,m2) ->
    paren_if in_cons (pr_motif true m1 ^" :: "^pr_motif false m2)

let rec pr_e pri = function
  | Int n -> let s = string_of_int n in if n<0 then "("^s^")" else s
  | Bool b -> pr_bool b
  | Var v -> v
  | Pair (e1,e2) -> "("^pr_e 0 e1^", "^pr_e 0 e2^")"
  | Nil -> "[]"
  | Cons (e,l) ->
    let p = pri_cons in
    paren_if (p < pri) (pr_e (p+1) e ^" :: "^ pr_e p l)
  | App (Var v, Pair (e1,e2)) when pri_o2 v <> 0 ->
    let p = pri_o2 v in
    paren_if (p < pri) (pr_e p e1 ^ " " ^ v ^ " " ^ pr_e (p+1) e2)
  | App (e1,e2) ->
    let p = pri_app in
    paren_if (p < pri) (pr_e p e1 ^" "^ pr_e (p+1) e2)
  | Fun [MVar v, e] ->
    let p = pri_fun in
    paren_if (p < pri) ("function "^v^" -> "^pr_e p e)
  | Fun l ->
    let p = pri_fun in
    let pr_branche (m,e) = pr_motif false m ^ " -> " ^ pr_e 0 e in
    paren_if (p < pri)
      ("function\n| " ^ String.concat "\n| " (List.map pr_branche l))
  | Let (d,e) ->
    let p = pri_let in
    paren_if (p < pri) (pr_def d ^ " in\n" ^ pr_e p e)

and pr_def d =
  "let "^(if d.recursive then "rec "else "")^d.nom^" = "^
  pr_e 0 d.expr

let string_of_expr e = pr_e 0 e

let string_of_phrase = function
  | Expr e -> string_of_expr e
  | Def d -> pr_def d
