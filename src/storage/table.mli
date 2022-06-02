type t
type parent = t
type name = string

module Iu : sig
  type t

  val make : string -> string -> Value_type.t -> t
  val eq : t -> t -> bool
  val show : t -> string
end

module Iter : sig
  type t

  val make : Wired_tiger.session_ref -> parent -> t
  val next : t -> Tuple.t option
  val to_list : t -> Tuple.t list
end

val name : t -> name
val schema : t -> Schema.t
val create : name -> Schema.t -> t
val insert : Wired_tiger.session_ref -> t -> Tuple.t -> unit
val bulk_insert : Wired_tiger.session_ref -> t -> Tuple.t list -> unit
val ius : t -> Iu.t list
