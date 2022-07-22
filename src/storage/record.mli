open Utils

module Id : sig
  type t = Unsigned.UInt64.t

  val zero : t
end

module Data : sig
  type t = Record_buffer.t

  val parse : Schema.t -> sep:char -> t -> string -> unit
  val copy_to : t -> Schema.t -> t -> Schema.t -> unit
  val show : t -> Schema.t -> string
end

type t = Id.t * Data.t
