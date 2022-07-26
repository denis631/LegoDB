open Utils

type t = Int of int64 | String of string

val ty : t -> Value_type.t
val parse_and_write : Value_type.t -> string -> (t -> unit) -> unit
