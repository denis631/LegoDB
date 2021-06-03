open Storage

type exec_ctx = unit

type match_expr_value_placeholder =
  | Const of Value.t
  | TableAttr of Schema.column_name

type match_expr_annot =
  | And of match_expr_annot * match_expr_annot
  | Or of match_expr_annot * match_expr_annot
  | Eq of match_expr_value_placeholder * match_expr_value_placeholder

type match_expr =
  | And of match_expr * match_expr
  | Or of match_expr * match_expr
  | Eq of Value.t * Value.t

(* build the match_expr by placing data in the placeholders *)

let rec make_match_expr annot ctx =
  match annot with
  | And (e1, e2) ->
      And (make_match_expr e1 ctx, make_match_expr e2 ctx)
  | Or (e1, e2) ->
      Or (make_match_expr e1 ctx, make_match_expr e2 ctx)
  | Eq (_, _) ->
      (* TODO: retrieve the real values *)
      Eq (Integer 1, Integer 1)


let rec eval_match = function
  | And (e1, e2) ->
      eval_match e1 && eval_match e2
  | Or (e1, e2) ->
      eval_match e1 || eval_match e2
  | Eq (v1, v2) ->
      Value.eq v1 v2


(* TODO: provide an abstraction (data cursor) that delivers data to the operators *)
type tbl_scan_ctx = { mutable tuples : Tuple.t list }

type operator =
  | TableScan of tbl_scan_ctx (* TODO: this is too low level. *)
  | Selection of match_expr_annot * operator
  (* TODO: support * projection *)
  | Projection of Schema.column_name list * operator

let rec next ctx = function
  | TableScan tbl_scan_ctx ->
    ( match tbl_scan_ctx.tuples with
    | x :: xs ->
        tbl_scan_ctx.tuples <- xs ;
        Some x
    | [] ->
        None )
  | Selection (_, child) ->
      let tuple = next ctx child in
      (* TODO: eval the predicate *)
      tuple
      (* eval_match @@ make_match_expr preds ctx *)
  | Projection (_, child) ->
      let tuple = next ctx child in
      tuple
