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
      (Arg.Unit (fun () -> Options.set_clean_types true))
      " Don't display every variable, but only the last type."
    ;

    write_options
      ["--unsafe"]
      (Arg.Unit (fun () -> Options.set_unsafe true))
      " Disable the typechecking"
    ;
   
  ]

(* does nothing when encounter an anonymous option *)
let anon_fun = Options.set_file

(* [parse ()] parse every cli arguments *)
let parse () =
  Arg.(parse (align options) anon_fun "main [options]")
