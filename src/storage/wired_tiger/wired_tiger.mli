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
  type record_id = Unsigned.UInt64.t
  type record_data = Bytearray.t
  type t = record_id * record_data

  val insert_one :
    session_ref:session_ref -> tbl_name:string -> record:t -> unit

  val bulk_insert :
    session_ref:session_ref ->
    tbl_name:string ->
    records:t Core.Sequence.t ->
    unit

  val delete_one :
    session_ref:session_ref -> tbl_name:string -> key:record_id -> unit

  val lookup_one :
    session_ref:session_ref -> tbl_name:string -> key:record_id -> t option

  val scan : session_ref:session_ref -> tbl_name:string -> unit -> t option
end
