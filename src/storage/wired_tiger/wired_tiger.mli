type t

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

module TblCreationConfig : sig
  type t = string
end

val create_tbl : db:t -> tbl_name:tbl_name -> config:TblCreationConfig.t -> unit

(* (\* TODO: records operations *\) *)
val insert_record :
  db:t -> tbl_name:tbl_name -> key:string -> record:string -> unit
(* val lookup_one: t -> string -> string *)
