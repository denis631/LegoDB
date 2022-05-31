type session

module IsolationLevelConfig : sig
  type t = Snapshot | ReadCommitted
end

val init_and_open_session :
  path:string ->
  config:string ->
  isolation_config:IsolationLevelConfig.t ->
  session

(* (\* TODO: wrap into a submodule? *\) *)
(* val begin_txn : t -> unit *)
(* val commit_txn : t -> unit *)
(* val abort_txn : t -> unit *)

(* (\* TODO: collection ops *\) *)
val create_tbl : session:session -> tbl_name:string -> config:string -> unit

(* Operations on records *)
module Record : sig
  type t = Bytearray.t

  val bulk_insert :
    session:session -> tbl_name:string -> keys_and_records:(t * t) list -> unit

  val insert_one :
    session:session -> tbl_name:string -> key:t -> record:t -> unit

  val lookup_one : session:session -> tbl_name:string -> key:t -> t option
  val scan : session:session -> tbl_name:string -> unit -> t option
end
