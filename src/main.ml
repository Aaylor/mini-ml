
open Logger

let source_file () = match Options.File.get () with
  | ""  -> failwith "No file given."
  | src -> src

(* let typechecker phrase = *)
(*   if not (Options.Unsafe.get ()) then begin *)
(*     basic "\n\n(\*** typage ***\)\n"; *)
(*     let _, env = Typechecker.infer_phrase phrase in *)
(*     basic "%s" (Typechecker.string_of_env env); *)
(*   end *)

let typechecker phrase =
  snd (Typechecker.infer_phrase phrase)

let interpreter file =
  basic "\n(*** interpreter ***)\n";
  let stream = Stream.of_channel (open_in file) in
  let rec read_phrase () =
    try Stream.empty stream
    with Stream.Failure -> begin
        let phrase = Parser.lire_phrase stream in
        Printf.printf "%s" (Printer.string_of_phrase phrase);
        let result = Interpreter.(interpret (init_env ()) phrase) in
        basic "%s%!" (Interpreter.string_of_result result);
        Printf.printf "End.\n";
        read_phrase ()
      end
  in
  read_phrase ()

let interactive () =
  assert false

let main =
  Cli_parser.parse ();
  if Options.Interactive.get () then
    interactive ()
  else if Options.Interpreter.get () then
    interpreter (source_file ())
    
  
  (* let phrase = Parser.lire_phrase (Stream.of_channel (open_in file)) in *)
  (* typechecker  phrase; *)
  (* interpreter  phrase *)
