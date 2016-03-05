open Types

type schema = { quantifier : VarSet.t; ty : expression_type }

let empty ty = { quantifier = VarSet.empty; ty }

let string_of_schema { quantifier; ty } =
  let qs =
    if VarSet.is_empty quantifier then ""
    else Printf.sprintf " %% { ∀ %s }"
        (String.concat ", " (VarSet.to_list quantifier))
  in
  Printf.sprintf "%s%s" (string_of_expression_type ty) qs
