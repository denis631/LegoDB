type t

val create : unit -> t
val db_session_ref : t -> Wired_tiger.session_ref
val tbls : t -> Table.t list
val create_tbl : t -> Table.t -> unit
