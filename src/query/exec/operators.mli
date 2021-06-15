open Storage

type exec_ctx = unit

(* TODO: provide an abstraction (data cursor) that delivers data to the operators *)
type tbl_scan_ctx =
  { tbl : Table.t
  ; mutable tuple_idx : int
  ; attr_idxs : int list
  }

type proj_attrs = Table.Iu.t list

type operator =
  | TableScan of tbl_scan_ctx (* TODO: this is too low level. *)
  | Selection of Match.Expr.bool * operator
  | Projection of proj_attrs * operator

val prepare : Table.Iu.t list -> operator -> operator

val next : exec_ctx -> operator -> Tuple.t option
