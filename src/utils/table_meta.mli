open Marshaller

type t = {
  name : string;
  schema : Schema.t;
  indexes : Index.t list;
  mutable tid : Unsigned.UInt64.t [@default Unsigned.UInt64.zero];
}
[@@deriving make, show]

type meta = t

(* Need this marshaller in order to write table metadata into catalog table  *)
module Marshaller : Marshaller with type t = meta and type v = Bytearray.t
