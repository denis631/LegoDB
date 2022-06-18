open Utils
open Core

type column_name = string [@@deriving sexp, show]
type column = column_name * Value_type.t [@@deriving sexp, show]

(* TODO: should it be an iu instead? *)
type t = column list [@@deriving sexp, show]
type schema = t

module Marshaller : Marshaller with type t = schema and type v = string = struct
  type t = schema
  type v = string

  let marshal schema = sexp_of_t schema |> Sexp.to_string
  let unmarshal s = Sexp.of_string s |> t_of_sexp
end
