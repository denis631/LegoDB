open Storage
open Expr

type proj_attrs = Table.T.Iu.t list [@@deriving show]

type t =
  | TableScan of Table.T.Meta.t
  | Selection of Match.Expr.boolean * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t
  | Copy of string * string
  | CreateTbl of Table.T.Meta.t
  | DropTbl of Table.T.Meta.t list
[@@deriving show]
