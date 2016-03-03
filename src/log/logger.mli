
type level =
  | Error
  | Debug
  | Info
  | Basic

type color =
  | Red
  | Purple
  | Yellow
  | Default

val set_level : level -> unit

val error : ('a, out_channel, unit) format -> 'a

val debug : ('a, out_channel, unit) format -> 'a
  
val info : ('a, out_channel, unit) format -> 'a
 
val basic : ('a, out_channel, unit) format -> 'a
