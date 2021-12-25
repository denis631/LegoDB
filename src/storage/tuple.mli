(* TODO: make private? *)
type t = Value.t list

val eq : t -> t -> bool

val parse : Schema.t -> string -> t

val hash : t -> int64

val get : t -> int -> Value.t

val take : int -> t -> t

val extract_values : int list -> t -> t

val show : t -> string
