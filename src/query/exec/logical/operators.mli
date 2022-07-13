open Storage
open Expr

type proj_attrs = Attributes of Schema.t | All [@@deriving show]

type join_strategy = NLJ | HashJoin

type t =
  | TableScan of Table.Meta.t
  | Selection of t * Match.Expr.boolean
  | Projection of t * proj_attrs
  | Join of t * t * (Schema.t * Schema.t) * join_strategy
  | Copy of string * string
  | CreateTbl of Table.Meta.t
  | DropTbl of Table.Meta.t list
[@@deriving show]
