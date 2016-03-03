open Caml
open Types

type mgu = (expression_type * expression_type)

exception MguFailureException of string

let string_of_mgu (mgu : mgu) =
  Printf.sprintf "%s =?= %s\n"
    (string_of_expression_type (fst mgu))
    (string_of_expression_type (snd mgu))

let ( =?= ) : expression_type -> expression_type -> mgu =
  fun x y -> (x, y)

let rec occur_check (v1 : var) = function
  | BaseType _ ->
    false
  | Variable v2 ->
    var_eq_name v1 v2
  | Arrow (t1, t2)
  | Star  (t1, t2) ->
    occur_check v1 t1 || occur_check v1 t2
  | List t
  | Ref  t ->
    occur_check v1 t
  | Unit ->
    false

let rec mgu : mgu -> unit = fun mgu' ->
  Logger.debug "%s" (string_of_mgu mgu');
  let ok = () in
  match mgu' with
  (* Nothing to do *)
  | (BaseType b1, BaseType b2) when bt_eq b1 b2 ->
    Logger.debug "BaseType\n\n";
    ok
    
  (* Type variables are equals *)
  | (Variable v1, Variable v2) when var_eq v1 v2 ->
    Logger.debug "Var eq\n\n";
    ok
    
  (* Variable has no content *)
  | (Variable ({ content = None } as v), t)
  | (t, Variable ({ content = None} as v)) when not(occur_check v t) ->
    Logger.debug "One is None.\n";
    v.content <- Some t;
    Logger.debug "new result: %s\n\n" (string_of_mgu mgu');
    ok

  (* Variable has content *)
  | (Variable ({ content = Some(t1) }), t) ->
    Logger.debug "v1, t\n\n";
    mgu (t1 =?= t)

  | (t, Variable ({ content = Some(t1) })) ->
    Logger.debug "t, v2\n\n";
    mgu (t =?= t1)

  (* t1 -> t2   or   t1 * t2 *)
  | (Arrow(t1, t2), Arrow(t3, t4))
  | (Star(t1, t2), Star(t3, t4)) ->
    Logger.debug "Arrow or Star\n\n";
    mgu (t1 =?= t3);
    mgu (t2 =?= t4);
   
  | (List t1, List t2) ->
    Logger.debug "List\n\n";
    mgu (t1 =?= t2)

  | (Ref t1, Ref t2) ->
    Logger.debug "Ref\n\n";
    mgu (t1 =?= t2)

  (* Failure here, reached when there is an inconsistency in the mgu system *)
  | (x, y) ->
    raise (MguFailureException (
      Printf.sprintf "Failed to do mgu:\nt1: %s\nt2: %s\n"
        (string_of_expression_type x) (string_of_expression_type y)
    ))
