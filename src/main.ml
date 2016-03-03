
open Logger


let main =
  Cli_parser.parse ();
  let file = Options.file () in
  let prog = Parser.lire_phrase (Stream.of_channel (open_in file)) in
  
  basic "(*** phrase ***)\n";
  basic "%s" (Printer.string_of_phrase prog);

  if not (Options.unsafe ()) then begin
    basic "\n\n(*** typage ***)\n";
    let _, env = Typechecker.infer_phrase prog in
    basic "%s" (Typechecker.string_of_env env);
  end;

  basic "\n(*** interpreter ***)\n";
  let result = Interpreter.(interpret (init_env ()) prog) in
  basic "%s" (Interpreter.string_of_result result)
