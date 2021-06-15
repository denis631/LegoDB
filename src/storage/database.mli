type t

val create: unit -> t

val tbls: t -> Table.t list

val insert: t -> Table.t -> unit
