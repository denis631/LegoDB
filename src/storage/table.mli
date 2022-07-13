open Utils

type record = Row.t

module Meta : sig
  type t = { name : string; schema : Schema.t; indexes : Index.t list }
  [@@deriving make, show]

  type meta = t

  (* Need this marshaller in order to write table metadata into catalog table  *)
  module Marshaller : Marshaller with type t = meta and type v = record
end

module Crud : sig
  module Tbl : sig
    val exists : Wired_tiger.session_ref -> Meta.t -> bool
    val create : Wired_tiger.session_ref -> Meta.t -> unit
    val drop : Wired_tiger.session_ref -> Meta.t -> unit
  end

  module Record : sig
    val insert : Wired_tiger.session_ref -> Meta.t -> record -> unit

    val bulk_insert :
      Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t -> unit

    val read_all : Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t
    val delete : Wired_tiger.session_ref -> Meta.t -> record -> unit
  end
end
