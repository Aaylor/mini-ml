(** [write_options keys spec doc] is an helper to write options. *)
let write_options keys spec doc =
  List.map (fun n -> (n, spec, doc)) keys

(** program options *)
let options =
  List.flatten [

    write_options
      ["-v"; "--verbose"]
      (Arg.Int (function
           | 0 -> Logger.set_level Logger.Basic
           | 1 -> Logger.set_level Logger.Info
           | 2 -> Logger.set_level Logger.Debug
           | 3 -> Logger.set_level Logger.Error
           | _ -> failwith "Invalid integer value"))
      " Activate a verbose mode: default is basic."
    ;

    write_options
      ["-c"; "--clean"; "--clean-types"]
      (Arg.Unit (fun () -> Options.CleanTypes.set true))
      " Don't display every variable, but only the last type."
    ;

    write_options
      ["--unsafe"]
      (Arg.Unit (fun () -> Options.Unsafe.set true))
      " Disable the typechecking"
    ;

    write_options
      ["--no-interpreter"]
      (Arg.Unit (fun () -> Options.Interpreter.set false))
      " Disable the interpreter."
    ;

    write_options
      ["-i"; "--interactive"]
      (Arg.Unit (fun () -> Options.Interactive.set true))
      " Open a top-level for the program. If this option is set, interpreter \
       is disabled"
    ;
  ]

(* does nothing when encounter an anonymous option *)
let anon_fun = Options.File.set

(* [parse ()] parse every cli arguments *)
let parse () =
  Arg.(parse (align options) anon_fun "main [options]")
