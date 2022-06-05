open Utils

module type WiredTigerMarshaller = Marshaller with type v = Wired_tiger.Record.t

module type Tbl = sig
  type record = Tuple.t

  module Meta : sig
    type t
    type meta = t

    val make : string -> Schema.t -> t
    val name : t -> string
    val schema : t -> Schema.t

    (* Need this marshaller in order to write table metadata into catalog table  *)
    module Marshaller : Marshaller with type t = meta and type v = record
  end

  module Iu : sig
    type t

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
    val show : t -> string
  end

  module Crud : sig
    val exists : Wired_tiger.session_ref -> Meta.t -> bool
    val create : Wired_tiger.session_ref -> Meta.t -> unit
    val read_all : Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t
    val insert : Wired_tiger.session_ref -> Meta.t -> record -> unit

    val bulk_insert :
      Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t -> unit
  end

  val ius : Meta.t -> Iu.t list
end

module Make : functor (M : WiredTigerMarshaller with type t = Tuple.t) -> Tbl
module T : Tbl
