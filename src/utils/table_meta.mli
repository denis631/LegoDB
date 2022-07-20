open Marshaller

type t = { name : string; schema : Schema.t; indexes : Index.t list }
[@@deriving make, show]

type meta = t

(* Need this marshaller in order to write table metadata into catalog table  *)
module Marshaller : Marshaller with type t = meta and type v = Bytearray.t
