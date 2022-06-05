open Utils

(* TODO: make private? *)
type t = Value.t list
type tuple = t

val eq : t -> t -> bool
val parse : Schema.t -> string -> t
val hash : t -> int64
val get : t -> int -> Value.t
val take : int -> t -> t
val extract_values : int list -> t -> t
val show : t -> string

module Marshaller :
  Marshaller with type t = tuple and type v = Wired_tiger.Record.t
