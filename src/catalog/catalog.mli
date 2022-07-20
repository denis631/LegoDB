open Storage
open Utils

type t

(* TODO: remove it. add another singleton class instead *)
val instance : t
val session : t -> Database.Session.t
val create_tbl : t -> TableMeta.t -> unit (* TODO: result type? *)
val find_tbl : t -> string -> TableMeta.t
val drop_tbl : t -> TableMeta.t -> unit
