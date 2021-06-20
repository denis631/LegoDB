open Storage
open Expr

type exec_ctx = unit

type tbl_scan_ctx =
  { iter : Table.Iter.t
  ; ius : Table.Iu.t list
  ; attr_idxs : int list
  }

type proj_attrs = Table.Iu.t list

type t =
  | TableScan of tbl_scan_ctx
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t

val prepare : Table.Iu.t list -> t -> t

val next : exec_ctx -> t -> Tuple.t option
