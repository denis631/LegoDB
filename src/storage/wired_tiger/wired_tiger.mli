type t
type bin_repr_t = Bytearray.t

module IsolationLevelConfig : sig
  type t = Snapshot | ReadCommitted
end

val init_and_open_session :
  path:string -> config:string -> isolation_config:IsolationLevelConfig.t -> t

(* (\* TODO: wrap into a submodule? *\) *)
(* val begin_txn : t -> unit *)
(* val commit_txn : t -> unit *)
(* val abort_txn : t -> unit *)

(* (\* TODO: collection ops *\) *)
val create_tbl : session:t -> tbl_name:string -> config:string -> unit

(* Operations on records *)
val bulk_insert :
  session:t ->
  tbl_name:string ->
  keys_and_records:(bin_repr_t * bin_repr_t) list ->
  unit

val insert_record :
  session:t -> tbl_name:string -> key:bin_repr_t -> record:bin_repr_t -> unit

val lookup_one :
  session:t -> tbl_name:string -> key:bin_repr_t -> bin_repr_t option

val scan : session:t -> tbl_name:string -> unit -> bin_repr_t option
