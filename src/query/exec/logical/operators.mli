open Storage
open Expr

type proj_attrs = Table.RegularTbl.Iu.t list

type t =
  | TableScan of Table.RegularTbl.meta
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t

val show : t -> string
