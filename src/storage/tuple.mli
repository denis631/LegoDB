open Utils

(* TODO: make private? *)
type t = Value.t list [@@deriving hash, compare, equal, sexp]
type tuple = t

val parse : Schema.t -> string -> sep:char -> t
val get : t -> int -> Value.t
val extract_values : int list -> t -> t
val show : t -> string

module Marshaller :
  Marshaller with type t = tuple and type v = Wired_tiger.Record.t
