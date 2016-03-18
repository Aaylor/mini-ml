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


module VarSet : sig
  include Set.S with type elt := string
  val to_list : t -> string list
end

val et_eq : expression_type -> expression_type -> bool

val bt_eq : base_type -> base_type -> bool

val var_eq : var -> var -> bool

val var_eq_name : var -> var -> bool

val string_of_expression_type : expression_type -> string
