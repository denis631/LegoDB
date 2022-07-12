open Storage
open Expr

type proj_attrs = Schema.t [@@deriving show]

type t =
  | TableScan of Table.Meta.t
  | Selection of Match.Expr.boolean * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t
  | Join of t * t * (Schema.t * Schema.t)
  | Copy of string * string
  | CreateTbl of Table.Meta.t
  | DropTbl of Table.Meta.t list
[@@deriving show { with_path = false }]
