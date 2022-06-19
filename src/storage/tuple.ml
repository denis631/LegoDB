open Utils
open Core

type t = Value.t list [@@deriving hash, compare, equal, sexp]
type tuple = t

let parse schema ~sep data  =
  String.split ~on:sep data
  |> List.map2_exn ~f:Value.parse (List.map ~f:snd schema)

let get = List.nth_exn

let extract_values idxs =
  List.filteri ~f:(fun i _ -> List.exists ~f:(( = ) i) idxs)

let show t = String.concat ~sep:"," (List.map ~f:Value.show t)

module Marshaller :
  Marshaller with type t = tuple and type v = Wired_tiger.Record.t = struct
  type t = tuple
  type v = Wired_tiger.Record.t

  let marshal obj = Bytearray.marshal obj []
  let unmarshal bytearray = Bytearray.unmarshal bytearray 0
end
