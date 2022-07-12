type t = Tuple_buffer.t

val parse : Schema.t -> sep:char -> t -> string -> unit

val copy_tuple : t -> Schema.t -> Tuple_buffer.t -> Schema.t -> unit

val show : t -> Schema.t -> string
