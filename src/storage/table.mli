module type Record = sig
  type t

  val marshal : t -> Wired_tiger.Record.t
  val unmarshal : Wired_tiger.Record.t -> t
end

(* TODO: maybe just call this sig T *)
module type Tbl = sig
  type record

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

  module Iter : sig
    type t

    val make : Wired_tiger.session_ref -> Meta.t -> t
    val next : t -> record option
    val to_list : t -> record list
  end

  module Crud : sig
    val exists : Wired_tiger.session_ref -> Meta.t -> bool
    val create : Wired_tiger.session_ref -> Meta.t -> unit
    val insert : Wired_tiger.session_ref -> Meta.t -> record -> unit
    val bulk_insert : Wired_tiger.session_ref -> Meta.t -> record list -> unit
  end

  val ius : Meta.t -> Iu.t list
end

(* TODO: remove Record module and have only Marshaller *)
module Make : functor (R : Record) -> Tbl

module type RegularTbl = Tbl with type record = Tuple.t
module type CatalogTbl = Tbl with type record = string * Schema.t

module RegularTbl : RegularTbl
module CatalogTbl : CatalogTbl
