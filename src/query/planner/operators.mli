open Expr
open Utils

module TableMeta = Table_meta

type proj_attrs = Attributes of Schema.t | All [@@deriving show]

type join_strategy = NLJ | HashJoin

type t =
  | TableScan of TableMeta.t
  | Selection of t * Match.Expr.boolean
  | Projection of t * proj_attrs
  | Join of t * t * (Schema.t * Schema.t) * join_strategy
  | Copy of string * string
  | CreateTbl of TableMeta.t
  | DropTbl of TableMeta.t list
[@@deriving show]
