open Marshaller

type t = {
  name : string;
  schema : Schema.t;
  indexes : Index.t list;
  mutable tid : Unsigned.UInt64.t [@default Unsigned.UInt64.zero];
}
[@@deriving make, show]

type meta = t

module Marshaller : Marshaller with type t = meta and type v = Bytearray.t =
struct
  type t = meta
  type v = Bytearray.t

  let marshal meta : Bytearray.t = Bytearray.marshal meta []
  let unmarshal record : meta = Bytearray.unmarshal record 0
end
