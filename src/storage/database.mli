type t

val create : unit -> t
val db_session : t -> Wired_tiger.session
val tbls : t -> Table.t list
val create_tbl : t -> Table.t -> unit
