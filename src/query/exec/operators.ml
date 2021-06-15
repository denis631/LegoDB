open Storage
open BatteriesExceptionless

type exec_ctx = unit

type tbl_scan_ctx =
  { tbl : Table.t
  ; mutable tuples : Storage.Tuple.t list
  ; attr_idxs : int list
  }

type proj_attrs = Table.Iu.t list

type operator =
  | TableScan of tbl_scan_ctx (* TODO: this is too low level. *)
  | Selection of Match.Expr.bool * operator
  | Projection of proj_attrs * operator

(* | CrossProduct of operator * operator *)
(* TODO: should be created by joining predicate and cross product *)
(* | InnerJoin of match_expr_annot * operator * operator *)

(* TODO: for pushing the attributes to read *)
let rec prepare ius = function
  | TableScan tbl_scan_ctx ->
      let get_idx idx iu =
        if List.exists (Table.Iu.eq iu) ius then Some idx else None
      in
      let attr_idxs = List.filteri_map get_idx @@ Table.ius tbl_scan_ctx.tbl in
      TableScan
        { tbl = tbl_scan_ctx.tbl; tuples = tbl_scan_ctx.tuples; attr_idxs }
  | Selection (expr, op) ->
      let pred_ius = Match.Expr.ius (Match.Expr.BoolExpr expr) in
      let ius = List.unique @@ ius @ pred_ius in
      let iu_idx_map =
        let tbl : (Table.Iu.t, int) Hashtbl.t =
          Hashtbl.create @@ List.length pred_ius
        in
        let find_idx iu =
          match List.index_of iu ius with
          | Some i ->
              Hashtbl.add tbl iu i
          | None ->
              ()
        in
        List.iter find_idx pred_ius ;
        tbl
      in
      ( match Match.Expr.prepare iu_idx_map (Match.Expr.BoolExpr expr) with
      | BoolExpr matchExpr ->
          Selection (matchExpr, prepare ius op)
      | _ ->
          failwith "wrong implementation" )
  | Projection (ius, op) ->
      Projection (ius, prepare ius op)


let rec next ctx = function
  | TableScan tbl_scan_ctx ->
    ( match tbl_scan_ctx.tuples with
    | x :: xs ->
        tbl_scan_ctx.tuples <- xs ;
        Some (Storage.Tuple.extract_values tbl_scan_ctx.attr_idxs x)
    | [] ->
        None )
  | Selection (expr, child) ->
      let rec probe () =
        match next ctx child with
        | Some tuple ->
            let is_true = Value.eq @@ Value.Bool true in
            if is_true @@ Match.Expr.eval tuple @@ Match.Expr.BoolExpr expr
            then Some tuple
            else probe ()
        | None ->
            None
      in
      probe ()
  | Projection (_, child) ->
      next ctx child
