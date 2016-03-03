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


type motif =
  | MVar of string
  | MBool of bool
  | MInt of int
  | MPair of motif * motif
  | MNil
  | MCons of motif * motif

type expr =
   | Var of string              (** variable *)
   | Bool of bool               (** constante booleenne *)
   | Int of int                 (** constante entiere *)
   | Fun of (motif * expr) list (** fonction definie via une liste de cas *)
   | App of expr * expr         (** application *)
   | Let of definition * expr   (** let ... in ... *)
   | Pair of expr * expr        (** paire (e1,e2) *)
   | Nil                        (** liste vide *)
   | Cons of expr * expr        (** liste x::l *)

and definition =
  { recursive: bool;
    nom: string;
    expr: expr }

type phrase =
  | Expr of expr
  | Def of definition
