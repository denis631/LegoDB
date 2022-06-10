open Utils
open BatteriesExceptionless

type t = Value.t list
type tuple = t

let eq lhs rhs = not @@ List.exists2 (neg2 Value.eq) lhs rhs

let parse schema data ~sep =
  String.split_on_char sep data |> List.map2 Value.parse (List.map snd schema)

let hash t = List.map Value.hash t |> List.reduce Int64.logxor |> Option.get
let get = List.nth
let take = List.take
let extract_values idxs = List.filteri (fun i _ -> List.exists (( = ) i) idxs)
let show vals = String.concat " | " @@ List.map Value.show vals

module Marshaller :
  Marshaller with type t = tuple and type v = Wired_tiger.Record.t = struct
  type t = tuple
  type v = Wired_tiger.Record.t

  let marshal obj = Bytearray.marshal obj []
  let unmarshal bytearray = Bytearray.unmarshal bytearray 0
end
