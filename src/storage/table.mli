module type Record = sig
  type t

  val marshal : t -> Wired_tiger.Record.t
  val unmarshal : Wired_tiger.Record.t -> t
end

(* TODO: maybe just call this sig T *)
module type Tbl = sig
  (* TODO: introduce Meta module *)
  type meta
  type record

  module Iu : sig
    type t

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
    val show : t -> string
  end

  module Iter : sig
    type t

    val make : Wired_tiger.session_ref -> meta -> t
    val next : t -> record option
    val to_list : t -> record list
  end

  val name : meta -> string
  val schema : meta -> Schema.t
  val create_meta : string -> Schema.t -> to_key:(record -> record) -> meta
  (* TODO: fix this disaster and what about auto incrementing ids, so no specific record field *)

  (* TODO: introduce a CRUD module *)
  val exists : Wired_tiger.session_ref -> meta -> bool
  val create : Wired_tiger.session_ref -> meta -> unit
  val insert : Wired_tiger.session_ref -> meta -> record -> unit
  val bulk_insert : Wired_tiger.session_ref -> meta -> record list -> unit
  val ius : meta -> Iu.t list
end

(* TODO: remove Record module and have only Marshaller *)
module Make : functor (R : Record) -> Tbl

module type RegularTbl = Tbl with type record = Tuple.t
module type CatalogTbl = Tbl with type record = string * Schema.t

module RegularTbl : RegularTbl
module CatalogTbl : CatalogTbl
