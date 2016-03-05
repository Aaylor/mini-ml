open Caml


(* Environment *)

type global_env = {
  tenv : Typechecker.env;
  ienv : Interpreter.env;
}

let initial_env () = {
  tenv = Typechecker.initial_environment ();
  ienv = Interpreter.init_env ()
}

let print_result ?(typ = None) ?(value = None) phrase =
  let variable = match phrase with
    | Expr _ -> "val -"
    | Def  d  -> "val " ^ d.nom
  in
  let typ = match typ with
    | None -> " : _"
    | Some ty -> Printf.sprintf " : %s" (Types.string_of_expression_type ty)
  in
  let value = match value with
    | None -> " = _"
    | Some v -> Printf.sprintf " = %s" (Interpreter.string_of_value v)
  in
  Printf.printf "%s%s%s\n\n" variable typ value

let process_phrase print_phrase env phrase =
  if print_phrase then begin
    Format.fprintf Format.std_formatter "# @[<hov 2>%a@]@\n%!"
      Printer.fmt_of_phrase phrase
  end;
  try
    let (tenv, typ) =
      if Options.Unsafe.get() then
        (env.tenv, None)
      else
        let (e, t) = Typechecker.infer_phrase env.tenv phrase in
        (e, Some t)
    in
    let (ienv, value) =
      if (Options.Interpreter.get ()) || (Options.Interactive.get ()) then
        let (e, v) = Interpreter.interpret env.ienv phrase in
        (e, Some v)
      else
        (env.ienv, None)
    in
    print_result ~typ ~value phrase;
    { tenv; ienv }
  with
  | Mgu.MguFailureException why ->
    Printf.printf "Typechecking Error: %s\n" why;
    env
  | Failure e ->
    Printf.printf "Exception: Failure \"%s\"" e;
    env


(* INTERACTIVE MODE *)
let end_of_phrase line =
  try
    let idx = String.rindex line ';' in
    if (idx = 0 || String.get line (idx - 1) <> ';') then
      (line, false)
    else
      (String.sub line 0 (idx - 1), true)
  with Not_found ->
    (line, false)
  
let read_line_opt () =
  try Some (read_line ()) with End_of_file -> None

let rec read_phrase ?(buffer = Buffer.create 13) ?(break = false) () =
  Printf.printf "%s " (if break then " " else "#");
  match read_line_opt () with
  | Some line ->
    let line, eol = end_of_phrase line in
    Buffer.add_string buffer line;
    if eol then 
      Some (Buffer.contents buffer)
    else
      read_phrase ~buffer ~break:true ()
  | None -> None

let rec process_interactive env =
  match read_phrase () with
  | Some phrase_str ->
    let phrase = match Parser.lire_phrase (Stream.of_string phrase_str) with
      | [x] -> x
      | _ -> assert false
    in
    let env' = process_phrase false env phrase in
    process_interactive env'
  | None ->
    env

let interactive () =
  Printf.printf "\tMini-Caml - Projet Typage 2016\n\n";
  let env = initial_env () in
  ignore @@ process_interactive env


(* BATCH MODE *)

let get_phrases file =
  let channel = open_in file in
  let stream  = Stream.of_channel channel in
  let result  = Parser.lire_phrase stream in
  close_in channel;
  result

let rec batch_aux env = function
  | [] ->
    ()
  | x :: xs ->
    let env' = process_phrase true env x in
    batch_aux env' xs

let batch file =
  let phrases = get_phrases file in
  batch_aux (initial_env ()) phrases

