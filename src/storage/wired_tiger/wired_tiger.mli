type t
type bin_repr_t = Bytearray.t

val init : path:string -> config:string -> t

module IsolationLevelConfig : sig
  type t = Snapshot | ReadCommitted
end

val open_session : db:t -> config:IsolationLevelConfig.t -> t

(* (\* TODO: wrap into a submodule? *\) *)
(* val begin_txn : t -> unit *)
(* val commit_txn : t -> unit *)
(* val abort_txn : t -> unit *)

(* (\* TODO: collection ops *\) *)
type tbl_name = string

val create_tbl : db:t -> tbl_name:tbl_name -> config:string -> unit

(* TODO: records operations *)
val insert_record :
  db:t -> tbl_name:tbl_name -> key:bin_repr_t -> record:bin_repr_t -> unit

val lookup_one :
  db:t -> tbl_name:tbl_name -> key:bin_repr_t -> bin_repr_t option

val scan : db:t -> tbl_name:tbl_name -> unit -> (bin_repr_t * bin_repr_t) option
