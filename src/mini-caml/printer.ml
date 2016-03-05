
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

let surround b fmt s =
  Format.fprintf fmt "%s%t%s"
    (if b then "(" else "") s (if b then ")" else "")

let rec print_motif cons fmt = function
  | MVar v  ->
    Format.fprintf fmt "%s" v
  | MBool b ->
    Format.fprintf fmt "%B" b
  | MInt n  ->
    Format.fprintf fmt "%d" n
  | MPair (m1, m2) ->
    Format.fprintf
      fmt "@[(%a, %a)@]"
      (print_motif false) m1 (print_motif false) m2
  | MNil ->
    Format.fprintf fmt "[]"
  | MCons (m1, m2) ->
    Format.fprintf
      fmt "%a" (surround cons) (fun fmt ->
          Format.fprintf fmt "@[%a :: %a@]"
            (print_motif true) m1 (print_motif false) m2
        )


let rec print_expression priority fmt expr =
  let psurround p f = Format.fprintf fmt "%a" (surround (p < priority)) f in
  match expr with
  | Var v ->
    Format.fprintf fmt "%s" v
  | Bool b ->
    Format.fprintf fmt "%B" b
  | Int i ->
    Format.fprintf fmt "%a" (surround (i < 0)) (fun fmt ->
        Format.fprintf fmt "%d" i
      )
  | Fun [MVar v, e] ->
    psurround pri_fun (fun fmt ->
        Format.fprintf fmt "function %s -> %a" v
          (print_expression pri_fun) e
      )
  | Fun l ->
    let print_branch fmt (motif, expr) =
      Format.fprintf fmt "@\n| %a -> @[<hov 2>%a@]"
        (print_motif false) motif
        (print_expression 0) expr
    in
    psurround pri_fun (fun fmt ->
        Format.fprintf fmt "function%t"
          (fun fmt -> List.iter (print_branch fmt) l)
      )
  | App (Var v, Pair (e1, e2)) when pri_o2 v <> 0 ->
    psurround (pri_o2 v) (fun fmt ->
        Format.fprintf fmt "%a %s %a"
          (print_expression (pri_o2 v)) e1
          v
          (print_expression (pri_o2 v + 1)) e2
      )
  | App (e1, e2) ->
    psurround pri_app (fun fmt ->
        Format.fprintf fmt "%a %a"
          (print_expression pri_app) e1
          (print_expression (pri_app + 1)) e2
      )
  | Let (d, e) ->
    psurround pri_let (fun fmt ->
        Format.fprintf fmt
          "%a in@\n%a"
          print_definition d
          (print_expression pri_let) e
      )
  | Pair (p1, p2) ->
    Format.fprintf fmt "(%a, %a)"
      (print_expression 0) p1 (print_expression 0) p2
  | Nil ->
    Format.fprintf fmt "[]"
  | Cons (x, xs) ->
    psurround pri_cons (fun fmt ->
        Format.fprintf fmt "%a :: %a"
          (print_expression (pri_cons + 1)) x
          (print_expression pri_cons) xs
      )

and print_definition fmt { recursive; nom; expr } =
  Format.fprintf fmt
    "@[<hov 2>let %s%s = %a@]"
    (if recursive then "rec " else "")
    nom
    (print_expression 0) expr


let fmt_of_expression fmt e = Format.fprintf fmt "%a" (print_expression 0) e
let fmt_of_phrase fmt = function
  | Expr e -> fmt_of_expression fmt e
  | Def d -> Format.fprintf fmt "%a" print_definition d

let with_format f =
  let buffer = Buffer.create 13 in
  let format = Format.formatter_of_buffer buffer in
  f format;
  Buffer.contents buffer

let string_of_expr e =
  with_format (fun fmt -> fmt_of_expression fmt e)

let string_of_phrase = function
  | Expr e -> string_of_expr e
  | Def d  -> with_format (fun fmt -> Format.fprintf fmt "%a" print_definition d)

