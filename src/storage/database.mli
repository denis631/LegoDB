type record_id = Record.Id.t
type record_data = Record.Data.t
type record = Record.t

module Session : sig
  type t
  type session_t = t

  module Crud : sig
    module Table : sig
      val exists : t -> string -> bool
      val create : t -> string -> unit
      val drop : t -> string -> unit
    end

    module Record : sig
      val insert : t -> string -> record -> unit
      val bulk_insert : t -> string -> record Core.Sequence.t -> unit
      val read_all : t -> string -> record Core.Sequence.t
      val delete : t -> string -> record_id -> unit
    end
  end

  module Cursor : sig
    type t

    val make : session_t -> string -> (t, [> `FailedCursorOpen ]) result
    val get_key : t -> (record_id, [> `FailedCursorGetKey ]) result
    val get_value : t -> (record_data, [> `FailedCursorGetValue ]) result
    val set_key : t -> record_id -> unit
    val set_value : t -> record_data -> unit
    val insert : t -> (unit, [> `FailedCursorInsert ]) result
    val remove : t -> (unit, [> `FailedCursorRemove ]) result
    val search : t -> record_id -> record_data option

    (* val search_near : t -> record_id -> unit *)
    val next : t -> (unit, [> `FailedCursorNext ]) result
    val close : t -> (unit, [> `FailedCursorClose ]) result
  end
end

val make : unit -> Session.t
