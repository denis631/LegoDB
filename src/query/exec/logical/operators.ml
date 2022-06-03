open Storage
open Expr

type proj_attrs = Table.RegularTbl.Iu.t list

type t =
  | TableScan of Table.RegularTbl.meta
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t
  | CrossProduct of t * t

let rec show = function
  | TableScan tbl -> Table.RegularTbl.name tbl
  | Selection (pred, op) ->
      "Selection ("
      ^ Match.Expr.show (Match.Expr.BoolExpr pred)
      ^ ", " ^ show op ^ ")"
  | Projection (_, op) -> "Projection (" ^ show op ^ ")"
  | CrossProduct (left, right) -> show left ^ " x " ^ show right
