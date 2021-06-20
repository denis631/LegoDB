open Storage
open Expr

type proj_attrs = Table.Iu.t list

type t =
  | TableScan of Table.t
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t
