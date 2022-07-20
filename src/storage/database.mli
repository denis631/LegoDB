type record_id = Record.Id.t
type record = Record.t

module Session : sig
  type t

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
end

val make : unit -> Session.t
