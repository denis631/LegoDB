open Storage

type exec_ctx = unit

type tbl_scan_ctx =
  { iter : Table.Iter.t
  ; ius : Table.Iu.t list
  ; attr_idxs : int list
  }

type proj_attrs = Table.Iu.t list

type operator =
  | TableScan of tbl_scan_ctx
  | Selection of Match.Expr.bool * operator
  | Projection of proj_attrs * operator

val prepare : Table.Iu.t list -> operator -> operator

val next : exec_ctx -> operator -> Tuple.t option
