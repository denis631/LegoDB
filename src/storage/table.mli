type t

type name = string

module Iu : sig
  type t

  val make : string -> string -> Value_type.t -> t

  val eq : t -> t -> bool
end

val name : t -> name

val schema : t -> Schema.t

val tuple_at_idx : t -> int -> Tuple.t option

val create : name -> Schema.t -> t

val insert : t -> Tuple.t -> unit

val ius : t -> Iu.t list
