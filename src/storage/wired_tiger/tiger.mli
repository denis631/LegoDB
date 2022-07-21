open Ctypes

type session_ref = Session.t ptr

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
end

(* Operations on records *)
module Record : sig
  type record_id = Unsigned.UInt64.t
  type record_data = Bytearray.t
  type t = record_id * record_data

  val scan : session_ref:session_ref -> tbl_name:string -> unit -> t option
end
