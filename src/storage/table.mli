(* TODO: reuse this module type in other places *)
module type Marshaller = sig
  type t
  type v

  val marshal : t -> v
  val unmarshal : v -> t
end

module type WiredTigerMarshaller = Marshaller with type v = Wired_tiger.Record.t

(* TODO: maybe just call this sig T *)
module type Tbl = sig
  type record = Tuple.t

  module Meta : sig
    type t

    (* TODO: fix this disaster (to_key) and what about auto incrementing ids, so no specific record field *)
    val make : string -> Schema.t -> (record -> record) -> t
    val name : t -> string
    val schema : t -> Schema.t
    val to_key : t -> record -> record
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
