open Storage
open Expr

type proj_attrs = Table.T.Iu.t list

type t =
  | TableScan of Table.T.Meta.t
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t

let rec show = function
  | TableScan tbl -> Table.T.Meta.name tbl
  | Selection (pred, op) ->
      "Selection ("
      ^ Match.Expr.show (Match.Expr.Bool pred)
      ^ ", " ^ show op ^ ")"
  | Projection (_, op) -> "Projection (" ^ show op ^ ")"
  | CrossProduct (left, right) -> show left ^ " x " ^ show right
