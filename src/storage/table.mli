open Utils

module type WiredTigerMarshaller = Marshaller with type v = Wired_tiger.Record.t

module type Tbl = sig
  type record = Tuple.t

  module Meta : sig
    type t [@@deriving show]
    type meta = t

    val make : string -> Schema.t -> Index.t list -> t
    val name : t -> string
    val schema : t -> Schema.t
    val indexes : t -> Index.t list

    (* Need this marshaller in order to write table metadata into catalog table  *)
    module Marshaller : Marshaller with type t = meta and type v = record
  end

  module Iu : sig
    type t [@@deriving show]

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
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

  val ius : Meta.t -> Iu.t list
end

module Make : functor (M : WiredTigerMarshaller with type t = Tuple.t) -> Tbl
module T : Tbl
