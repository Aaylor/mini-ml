open Caml
open Mgu
open Types

let fresh_variable =
  let id = ref 0 in
  fun pre -> incr id;
    Variable ({ name = pre ^ string_of_int !id; content = None })

let extract_name = function
  | Variable { name } -> name
  | _ -> assert false

type schema = { quantifier: VarSet.t; ty: expression_type }
type env = schema Typeenv.t

let empty_schema ty = { quantifier = VarSet.empty; ty }

let string_of_schema { quantifier; ty } =
  let qs =
    if VarSet.is_empty quantifier then ""
    else Printf.sprintf " %% { ∀ %s }"
        (String.concat ", " (VarSet.to_list quantifier))
  in
  Printf.sprintf "%s%s" (string_of_expression_type ty) qs

let string_of_env env =
  Typeenv.fold (fun k v acc ->
      Printf.sprintf "%s%s : %s\n" acc k (string_of_schema v)
    ) env ""





let initial_environment () : env =
  let rec mk = function
    | [x; y; z] -> Arrow(Star(x, y), z)
    | [x; y]    -> Arrow(x, y)
    | _ -> assert false
  in
  let primitive env (n,e,q) = Typeenv.add n {quantifier = q; ty = (mk e)} env in
  let bt x = BaseType x in
  let eqv  = fresh_variable "eq"   and neqv = fresh_variable "neq"
  and tref = fresh_variable "ref"  and bang = fresh_variable "bang"
  and set  = fresh_variable "set"
  and vfst = fresh_variable "fst1" and vsnd = fresh_variable "snd1"
  and wfst = fresh_variable "fst2" and wsnd = fresh_variable "snd2" in
  let vset e1 e2 =
    VarSet.(add (extract_name e2) (singleton (extract_name e1)))
  in
  List.fold_left primitive Typeenv.empty [
    ("+",  [bt Int; bt Int; bt Int], VarSet.empty);
    ("-",  [bt Int; bt Int; bt Int], VarSet.empty);
    ("*",  [bt Int; bt Int; bt Int], VarSet.empty);
    ("/",  [bt Int; bt Int; bt Int], VarSet.empty);
    ("<",  [bt Int; bt Int; bt Bool], VarSet.empty);
    ("<=", [bt Int; bt Int; bt Bool], VarSet.empty);
    (">",  [bt Int; bt Int; bt Bool], VarSet.empty);
    (">=", [bt Int; bt Int; bt Bool], VarSet.empty);
    ("fst", [vfst; vsnd; vfst], vset vfst vsnd);
    ("snd", [wfst; wsnd; wsnd], vset wfst wsnd);
    ("=" , [eqv; eqv; bt Bool], VarSet.singleton (extract_name eqv));
    ("<>", [neqv; neqv; bt Bool], VarSet.singleton (extract_name neqv));
    ("ref", [tref; Ref tref], VarSet.singleton (extract_name tref));
    ("!", [Ref bang; bang], VarSet.singleton (extract_name bang));
    (":=", [Ref set; set; Unit], VarSet.singleton (extract_name set))
  ]

let rec substitution subst = function
  | Variable { name; content } as v ->
    begin try List.assoc name subst with Not_found -> v end
  | BaseType ty    -> BaseType ty
  | Arrow (t1, t2) -> Arrow (substitution subst t1, substitution subst t2)
  | Star  (t1, t2) -> Star  (substitution subst t1, substitution subst t2)
  | List  ty       -> List  (substitution subst ty)
  | Ref   ty       -> Ref   (substitution subst ty)
  | Unit           -> Unit

let instance alpha =
  let alpha_list = VarSet.to_list alpha.quantifier in
  let beta = List.map (fun _ -> fresh_variable "inst") alpha_list in
  substitution
    (List.combine alpha_list beta)
    alpha.ty

let rec free_variables_typ = function
  | Variable v ->
    VarSet.singleton v.name
  | BaseType _ ->
    VarSet.empty
  | Arrow (e1, e2)
  | Star  (e1, e2) ->
    VarSet.union (free_variables_typ e1) (free_variables_typ e2)
  | List t
  | Ref t ->
    free_variables_typ t
  | Unit ->
    VarSet.empty

let rec free_variables_env (env : env) =
  Typeenv.fold (fun _ value acc ->
      VarSet.(union (diff (free_variables_typ value.ty) value.quantifier) acc)
    ) env VarSet.empty

let generalize body ty env =
  Logger.debug "%s" (Printer.string_of_expr body);
  if Expansivity.is_expansive body then begin
    Logger.debug "    --> is_expansive\n";
    empty_schema ty
  end else begin
    Logger.debug "    --> is NOT expansive\n";
    { quantifier =
        VarSet.diff (free_variables_typ ty) (free_variables_env env);
      ty
    }
  end

let rec infer_phrase (env : env) phrase =
  match phrase with
  | Expr e -> infer_expression env e
  | Def  d -> infer_definition env d

and infer_definition env def : env * expression_type =
  let (env', ty) =
    match def with
    | { recursive = false; nom; expr } ->
      let _, ty = infer_expression env expr in
      let env'  = Typeenv.add nom (empty_schema ty) env in
      (env', ty)
    | { recursive = true;  nom; expr } ->
      let arrow = Arrow (
          fresh_variable "arrow_left",
          fresh_variable "arrow_right"
        )
      in
      let env' = Typeenv.add nom (empty_schema arrow) env in
      let _, ty = infer_expression env' expr in
      mgu (arrow =?= ty);
      (env', ty)
  in
  Logger.debug "  Internal --  %s : %s\n"
    def.nom (string_of_expression_type ty);
  (env', ty)

and infer_expression env expr = match expr with
  | Var v ->
    let schema = Typeenv.find v env in
    env, instance schema
  | Bool _ ->
    env, BaseType Bool
  | Int _ ->
    env, BaseType Int
  | Fun cases ->
    infer_cases env cases
  | App (e1, e2) ->
    let _, ty_e1 = infer_expression env e1 in
    Logger.debug "Infer e1: %s\n" (string_of_expression_type ty_e1);
    let _, ty_e2 = infer_expression env e2 in
    Logger.debug "Infer e2: %s\n" (string_of_expression_type ty_e2);
    let fresh = fresh_variable "app" in
    Logger.debug "MGU OF: %s\n" (Printer.string_of_expr (App(e1, e2)));
    mgu (ty_e1 =?= Arrow(ty_e2, fresh));
    env, fresh
  | Let (def, body) ->
    let _, ty_def = infer_definition env def in
    let env' = Typeenv.add def.nom (generalize expr ty_def env) env in
    infer_expression env' body
  | Pair (e1, e2) ->
    let _, ty_e1 = infer_expression env e1 in
    let _, ty_e2 = infer_expression env e2 in
    env, Star (ty_e1, ty_e2)
  | Nil  ->
    let fresh = fresh_variable "nil" in
    env, List fresh
  | Cons (x, xs) ->
    let _, ty_x  = infer_expression env x in
    let _, ty_xs = infer_expression env xs in
    mgu (List ty_x =?= ty_xs);
    env, ty_xs
    
and bind_pattern env = function
  | MVar  v -> Typeenv.add v (empty_schema (fresh_variable "pattern")) env
  | MBool _ -> env
  | MInt  _ -> env
  | MPair (p1, p2) -> bind_pattern (bind_pattern env p1) p2
  | MNil -> env
  | MCons (x, xs) -> bind_pattern (bind_pattern env x) xs

and pattern_as_type env = function
  | MVar v -> (Typeenv.find v env).ty
  | MBool _ -> BaseType Bool
  | MInt _ -> BaseType Int
  | MPair (p1, p2) -> Star (pattern_as_type env p1, pattern_as_type env p2)
  | MNil -> List (fresh_variable "mnil")
  | MCons (x, _) -> List (pattern_as_type env x)

and infer_cases env cases =
  let input_ty = fresh_variable "cases_input" in
  let output_ty = fresh_variable "cases_output" in

  let rec extract_types = function
    | [] -> []
    | (pattern, expr) :: xs ->
      let env' = bind_pattern env pattern in
      let result =
        (pattern_as_type env' pattern, snd (infer_expression env' expr))
      in
      result :: extract_types xs
  in

  let inputs, outputs = List.split (extract_types cases) in

  (* A problem exists here: it depends on the position of nil list... *)
  Logger.debug "MGU OF BRANCHES:\n";
  List.iter (fun x -> mgu (x =?= input_ty)) inputs;
  List.iter (fun x -> mgu (x =?= output_ty)) outputs;
  
  env, Arrow (input_ty, output_ty)
