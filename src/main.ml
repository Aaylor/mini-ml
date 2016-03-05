
let source_file () = match Options.File.get () with
  | ""  -> failwith "No file given."
  | src -> src

let main =
  Cli_parser.parse ();
  if Options.Interactive.get () then
    Batch.interactive ()
  else if Options.Interpreter.get () then
    Batch.batch (source_file ())    
  
