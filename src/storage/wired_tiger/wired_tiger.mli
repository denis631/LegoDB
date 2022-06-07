type session_ref

val init_and_open_session : path:string -> config:string -> session_ref

(* Managing transactions *)
module Txn : sig
  val begin_txn : session_ref -> unit
  val commit_txn : session_ref -> unit
  val rollback_txn : session_ref -> unit
end

(* Operations on tables *)
module Table : sig
  val exists : session_ref:session_ref -> tbl_name:string -> bool

  val create :
    session_ref:session_ref -> tbl_name:string -> config:string -> unit

  val drop : session_ref:session_ref -> tbl_name:string -> config:string -> unit
end

(* Operations on records *)
module Record : sig
  type t = Bytearray.t

  val bulk_insert :
    session_ref:session_ref ->
    tbl_name:string ->
    keys_and_records:(t * t) list ->
    unit

  val insert_one :
    session_ref:session_ref -> tbl_name:string -> key:t -> record:t -> unit

  val delete_one : session_ref:session_ref -> tbl_name:string -> key:t -> unit

  val lookup_one :
    session_ref:session_ref -> tbl_name:string -> key:t -> t option

  val scan : session_ref:session_ref -> tbl_name:string -> unit -> t option
end
