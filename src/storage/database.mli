type t

val create : unit -> t
val db_session_ref : t -> Wired_tiger.session_ref
val catalog : t -> Catalog.t
val instance : t
val create_tbl : t -> Table.Meta.t -> unit
val drop_tbl : t -> Table.Meta.t -> unit
