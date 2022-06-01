type session_ref

val init_and_open_session : path:string -> config:string -> session_ref

(* (\* TODO: wrap into a submodule? *\) *)
(* val begin_txn : t -> unit *)
(* val commit_txn : t -> unit *)
(* val abort_txn : t -> unit *)

(* Operations on tables *)
module Table : sig
  val create :
    session_ref:session_ref -> tbl_name:string -> config:string -> unit
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

  val lookup_one :
    session_ref:session_ref -> tbl_name:string -> key:t -> t option

  val scan : session_ref:session_ref -> tbl_name:string -> unit -> t option
end
