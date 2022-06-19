open Storage
open Expr

type proj_attrs = Table.T.Iu.t list

type t =
  | TableScan of Table.T.Meta.t
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t
  | Copy of string * string
  | CreateTbl of Table.T.Meta.t
  | DropTbl of Table.T.Meta.t list

val show : t -> string
