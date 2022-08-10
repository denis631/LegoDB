module TableMeta = Utils.Table_meta

type t

(* TableMeta.t of the catalog table *)
val meta : TableMeta.t
val make : unit -> t
val set_catalog_tables : t -> TableMeta.t list -> unit
val create_tbl : t -> TableMeta.t -> unit (* TODO: result type? *)
val find_tbl : t -> string -> TableMeta.t
val drop_tbl : t -> TableMeta.t -> unit
