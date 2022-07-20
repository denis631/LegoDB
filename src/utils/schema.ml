open Core
open Marshaller

module Iu = struct
  type t = { table : string; column : string; ty : Value_type.t }
  [@@deriving compare, equal, hash, make, sexp, show { with_path = false }]
end

type t = Iu.t list [@@deriving compare, hash, equal, sexp, show]
type schema = t

let offset_to_attr schema iu =
  let rec calculate_offset acc = function
    | x :: _ when Iu.equal x iu -> acc
    | x :: xs -> calculate_offset (acc + Value_type.sizeof x.ty) xs
    | _ -> failwith ""
  in
  calculate_offset 0 schema

module Marshaller : Marshaller with type t = schema and type v = string = struct
  type t = schema
  type v = string

  let marshal schema = sexp_of_t schema |> Sexp.to_string
  let unmarshal s = Sexp.of_string s |> t_of_sexp
end
