type t = Row_buffer.t

val parse : Schema.t -> sep:char -> t -> string -> unit
val copy : t -> Schema.t -> Schema.t -> t
val copy_to : t -> Schema.t -> t -> Schema.t -> unit
val show : t -> Schema.t -> string
