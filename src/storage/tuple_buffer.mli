open Ctypes
open Utils

(* TODO: currently table is tied on Bigarray.Array1.t *)
type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type buffer = t

val make : int -> t
val length : t -> int
val clone : t -> t
val size_from_schema : Schema.t -> int
val get_ptr : t -> char ptr
val marshal : 'a -> t
val unmarshal : t -> int -> 'a

module Iterator : sig
  type t

  val make : buffer -> t
  val get_ptr : t -> char ptr
  val advance_by_offset : t -> int -> unit
  val advance_by_type : t -> Value_type.t -> unit
  val copy : t -> t -> int -> unit
  val write : t -> Value.C.t -> unit
  val show : t -> Value_type.t -> string
end
