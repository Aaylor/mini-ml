(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)

open Caml
open Genlex

let lire_operateur operateurs = parser
  | [< 'Kwd op when List.mem op operateurs >] -> op

let lire_operation lire_base operateurs =
  let rec lire_reste e1 = parser
  | [< op = lire_operateur operateurs;
       e2 = lire_base;
       e = lire_reste (App(Var op, Pair(e1, e2))) >] -> e
  | [< 'Int n when n < 0 && List.mem "-" operateurs;
       e = lire_reste (App(Var "-", Pair(e1, Caml.Int (-n)))) >] -> e
  | [< >] -> e1 in
  parser [< e1 = lire_base; e = lire_reste e1 >] -> e

let lire_infixe lire_base infixe construire_syntaxe flux =
  let rec lire_debut = parser
    [< e1 = lire_base; e2 = lire_reste e1 >] -> e2
  and lire_reste e1 = parser
  | [< 'Kwd op when op = infixe; e2 = lire_debut>] ->
      construire_syntaxe e1 e2
  | [< >] -> e1 in
  lire_debut flux

let rec motif flux =
  lire_infixe motif1 "," (fun m1 m2 -> MPair(m1, m2)) flux

and motif1 flux =
  lire_infixe motif_simple "::" (fun m1 m2 -> MCons(m1, m2)) flux

and motif_simple = parser
  | [< 'Ident id >] -> MVar id
  | [< 'Int n >] -> MInt n
  | [< 'Kwd "true" >] -> MBool true
  | [< 'Kwd "false" >] -> MBool false
  | [< 'Kwd "["; 'Kwd "]" >] -> MNil
  | [< 'Kwd "("; m = motif; 'Kwd ")" >] -> m

let rec expr_simple minus_ok = parser
  | [< 'Int i when minus_ok || i>=0 >] -> Caml.Int i
  | [< 'Kwd "true" >] -> Bool true
  | [< 'Kwd "false" >] -> Bool false
  | [< 'Ident id >] -> Var id
  | [< 'Kwd "["; 'Kwd "]" >] -> Nil
  | [< 'Kwd "("; e = expression; 'Kwd ")" >] -> e

and expr0 = parser
  | [< d = definition; 'Kwd "in"; e = expression >] -> Let(d, e)
  | [< 'Kwd "function"; liste = liste_de_cas >] ->
      Fun(liste)
  | [< 'Kwd "match"; e = expression; 'Kwd "with";
       liste = liste_de_cas >] ->
      App(Fun(liste), e)
  | [< es = expr_simple true; e = suite_d'applications es >] -> e

and suite_d'applications f = parser
  | [< arg = expr_simple false;
       e = suite_d'applications (App(f, arg)) >] -> e
  | [<>] -> f

and expr05 = parser
  | [< 'Kwd "!"; e = expr0 >] -> App (Var "!", e)
  | [< e = expr0 >] -> e

and expr1 flux =
  lire_operation expr05 ["*"; "/"] flux

and expr2 flux =
  lire_operation expr1 ["+"; "-"] flux

and expr3 flux =
  lire_operation expr2 ["="; "<>"; "<"; ">"; "<="; ">="] flux

and expr4 flux =
  lire_infixe expr3 "::" (fun e1 e2 -> Cons(e1, e2)) flux

and expr5 flux =
  lire_operation expr4 [":="] flux

and expr6 flux =
  lire_infixe expr5 ";" (fun e1 e2 ->
                        Let ({ recursive = false;
                               nom = "_";
                               expr = e1 }, e2)) flux

and expression flux =
  lire_infixe expr6 "," (fun e1 e2 -> Pair(e1, e2)) flux

and definition = parser
  | [< 'Kwd "let"; r = recursive; 'Ident nom; 'Kwd "="; e = expression >] ->
      {recursive = r; nom = nom; expr = e}
and recursive = parser
  | [< 'Kwd "rec" >] -> true
  | [< >] -> false

and liste_de_cas = parser
  | [< m = motif; 'Kwd "->"; e = expression; reste = autres_cas >] ->
      (m, e) :: reste

and autres_cas = parser
  | [< 'Kwd "|"; m = motif; 'Kwd "->"; e = expression;
       reste = autres_cas >] -> (m, e) :: reste
  | [< >] -> []

let rec phrase = parser
  | [< d = definition; p = fin_de_definition d  >] -> p
  | [< e = expression >] -> Expr e
and fin_de_definition d = parser
  | [< 'Kwd "in"; e = expression >] -> Expr (Let(d, e))
  | [< >] -> Def d

let rec top_level = parser
  | [< p = phrase; top = top_level >] -> p :: top
  | [< >] -> []

let analyseur_lexical =
  Genlex.make_lexer [
    "function"; "let"; "rec"; "in"; "match"; "with"; "true"; "false";
    "->"; "["; "]"; "("; ")"; "|"; ",";
    "*"; "/"; "-"; "+"; "="; "<>"; "<"; ">"; "<="; ">="; "::"; ";"; ":="; "!"
  ]

let lire_phrase flux =
  try
    top_level (analyseur_lexical flux)
  with e ->
    Printf.printf "Parse error at char %d\n%!" (Stream.count flux);
    raise e
