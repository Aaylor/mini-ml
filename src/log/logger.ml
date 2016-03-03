
type level =
  | Error
  | Debug
  | Info
  | Basic

let int_of_level = function
  | Error -> 0
  | Debug -> 1
  | Info  -> 2
  | Basic -> 3

type color =
  | Red
  | Purple
  | Yellow
  | Default

let string_of_color = function
  | Red     -> "\027[31m"
  | Purple  -> "\027[35m"
  | Yellow  -> "\027[33m"
  | Default -> "\027[39m"

let color_reset = "\027[0m"

let color_of_level = function
  | Error -> Red
  | Info  -> Yellow
  | Debug -> Purple
  | Basic -> Default

let current_level = ref Info

let set_level lvl = current_level := lvl

let header lvl =
  let mk str =
    Printf.sprintf "%s[%s]:%s\n"
      (string_of_color (color_of_level (lvl))) str color_reset
  in match lvl with
  | Error -> mk "ERROR"
  | Info  -> mk "INFO"
  | Debug -> mk "DEBUG"
  | Basic -> ""

let log lvl fmt =
  if int_of_level lvl >= int_of_level !current_level then
    Printf.kfprintf
      (fun chan -> Printf.fprintf chan "%s" color_reset)
      stdout
      ("%s" ^^ fmt) (string_of_color (color_of_level lvl))
  else
    Printf.ifprintf stdout fmt

let error fmt = log Error fmt
let debug fmt = log Debug fmt
let info  fmt = log Info  fmt
let basic fmt = log Basic fmt
