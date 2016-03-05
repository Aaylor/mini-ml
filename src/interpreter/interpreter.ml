open Caml
open Memory

module Env = Map.Make(struct
    type t = string
    let compare = compare
  end)

type env   = value Env.t 
 and value =
  | VInt       of int
  | VBool      of bool
  | VClosure   of env ref * (motif * expr) list
  | VTuple     of (value * value)
  | VList      of value list
  | VPrimitive of (value -> value)
  | VAddress   of Memory.address
  | VUnit

let memory = Memory.init_memory VUnit

let wrap_int i = VInt i
let wrap_bool b = VBool b

let unwrap_int = function VInt i -> i | _ -> assert false
let unwrap_bool = function VBool b -> b | _ -> assert false

(* Value helper *)
let rec string_of_motif = function
  | MVar v -> v
  | MBool b -> Printf.sprintf "%B" b
  | MInt i -> Printf.sprintf "%d" i
  | MPair (m1, m2) ->
    Printf.sprintf "(%s, %s)" (string_of_motif m1) (string_of_motif m2)
  | MNil  -> "[]"
  | MCons (m1, m2) ->
    Printf.sprintf "%s :: %s" (string_of_motif m1) (string_of_motif m2)

let rec string_of_value = function
  | VInt i ->
    Printf.sprintf "%d" i
  | VBool b ->
    Printf.sprintf "%B" b
  | VClosure (_, _) ->
    "λ. <closure>"
  | VTuple (v1, v2) ->
    Printf.sprintf "(%s, %s)"
      (string_of_value v1) (string_of_value v2)
  | VList vl ->
    Printf.sprintf "[ %s ]"
      (List.fold_right (fun elt acc ->
           string_of_value elt ^ ";" ^ acc) vl "")
  | VPrimitive _ ->
    "λ. <primitive>"
  | VAddress addr ->
    begin try
      let value = Memory.get addr memory in
      Printf.sprintf "{ contents = %s }" (string_of_value value)
    with _ ->
      "&<internal error>"
    end
  | VUnit ->
    "()"

let string_of_result v = Printf.sprintf "%s\n" (string_of_value v)



(* Environment helper *)
let init_env () =
  let primitive wrapper unwrapper op =
    VPrimitive (fun x -> match x with
        | VTuple(v1, v2) ->
          wrapper (op (unwrapper v1) (unwrapper v2))
        | _ ->
          assert false
      )
  in
  let eq = VPrimitive (fun x -> match x with
      | VTuple (VBool b, VBool b2) -> VBool(b = b2)
      | VTuple (VInt v1, VInt v2) -> VBool(v1 = v2)
      | _ -> assert false
    )
  in
  let neq = VPrimitive (fun x -> match x with
      | VTuple (VBool b, VBool b2) -> VBool(b <> b2)
      | VTuple (VInt v1, VInt v2) -> VBool(v1 <> v2)
      | _ -> assert false
    )
  in
  let primitives = [
    ("+",  primitive wrap_int  unwrap_int ( + ));
    ("-",  primitive wrap_int  unwrap_int ( - ));
    ("/",  primitive wrap_int  unwrap_int ( / ));
    ("*",  primitive wrap_int  unwrap_int ( * ));
    ("<",  primitive wrap_bool unwrap_int ( < ));
    ("<=", primitive wrap_bool unwrap_int ( <= ));
    (">",  primitive wrap_bool unwrap_int ( > ));
    (">=", primitive wrap_bool unwrap_int ( >= ));
    ("=",  eq);
    ("<>", neq);
    ("ref", VPrimitive (fun x -> VAddress (Memory.store x memory)));
    ("!", VPrimitive (function
         | VAddress a -> Memory.get a memory
         | _ -> assert false));
    (":=", VPrimitive (function
         | VTuple (VAddress a, value) -> Memory.update a value memory; VUnit
         | _ -> assert false));
    ("fst", VPrimitive (function
         | VTuple (v1, _) -> v1
         | _ -> assert false));
    ("snd", VPrimitive (function
         | VTuple (_, v2) -> v2
         | _ -> assert false))
  ] in
  List.fold_left (fun env elt ->
      Env.add (fst elt) (snd elt) env
    ) Env.empty primitives 



(* The interpreter itself *)
exception PatternBindingFailure of string

let rec interpret env phrase =
  match phrase with
  | Def definition ->
    interpret_definition env definition
  | Expr expression ->
    env, interpret_expression env expression

and interpret_definition env definition =
  match definition with
  | { recursive = false; nom; expr } ->
    let value = interpret_expression env expr in
    Env.add nom value env, value
  | { recursive = true; nom; expr  } ->
    let env' = Env.add nom (VInt 0) env in
    let value = interpret_expression env' expr in
    let env' = match value with
      | VClosure (env', _) -> env' := Env.add nom value !env'; !env'
      | _ -> env'
    in
    env', value

and interpret_expression env expression =
  match expression with
  | Var v  ->
    begin try
      Env.find v env
    with Not_found ->
      failwith (Printf.sprintf "%s not found." v)
    end
  | Bool b -> VBool b
  | Int i  -> VInt i
  | Fun ls -> VClosure(ref env, ls)
  | App (e1, e2) ->
    begin match interpret_expression env e1 with
    | VClosure (env', assocs) ->
      let v2 = interpret_expression env e2 in
      let env', next = function_bind_pattern !env' assocs v2 in
      interpret_expression env' next
    | VPrimitive f ->
      f (interpret_expression env e2)
    | _ ->
      assert false
    end
  | Let (def, e) ->
    let env', _ = interpret_definition env def in
    interpret_expression env' e
  | Pair (e1, e2) -> VTuple(interpret_expression env e1,
                            interpret_expression env e2)
  | Nil    -> VList []
  | Cons(e1, e2) ->
    begin match interpret_expression env e2 with
    | VList l -> VList ((interpret_expression env e1) :: l)
    | _ -> assert false
    end

and function_bind_pattern env assocs value =
  let rec aux = function
    | [] -> assert false (* by typing *)
    | (pat, expr) :: xs ->
      try
        bind_pattern env pat value, expr
      with PatternBindingFailure(_) ->
        aux xs
  in
  aux assocs

and bind_pattern env pat value =
  match pat, value with
  | MVar str, v -> Env.add str v env
  | MBool b1, VBool b2 when b1 = b2 -> env
  | MInt i, VInt i2 when i = i2 -> env
  | MPair(p1, p2), VTuple(v1, v2) ->
    bind_pattern (bind_pattern env p1 v1) p2 v2
  | MNil, VList [] -> env
  | MCons(m1, m2), VList l when l <> [] ->
    let head, tl = List.hd l, List.tl l in
    bind_pattern (bind_pattern env m1 head) m2 (VList tl)
  | pat, value ->
    let msg =
      Printf.sprintf "bind_pattern: incompatible.\npattern: %s\nvalue: %s\n"
        (string_of_motif pat) (string_of_value value)
    in
    raise (PatternBindingFailure msg)
