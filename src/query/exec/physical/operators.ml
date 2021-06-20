open Storage
open Expr
open BatteriesExceptionless

type exec_ctx = unit

type tbl_scan_ctx =
  { iter : Table.Iter.t
  ; ius : Table.Iu.t list
  ; attr_idxs : int list
  }

type proj_attrs = Table.Iu.t list

type t =
  | TableScan of tbl_scan_ctx (* TODO: this is too low level. *)
  | Selection of Match.Expr.bool * t
  | Projection of proj_attrs * t

(* | CrossProduct of operator * operator *)
(* TODO: should be created by joining predicate and cross product *)
(* | InnerJoin of match_expr_annot * operator * operator *)

(* TODO: for pushing the attributes to read *)
let rec prepare ius = function
  | TableScan tbl_scan_ctx ->
      let get_idx idx iu =
        if List.exists (Table.Iu.eq iu) ius then Some idx else None
      in
      let attr_idxs = List.filteri_map get_idx tbl_scan_ctx.ius in
      TableScan { iter = tbl_scan_ctx.iter; ius = tbl_scan_ctx.ius; attr_idxs }
  | Selection (expr, op) ->
      let pred_ius = Match.Expr.ius (Match.Expr.BoolExpr expr) in
      let ius = List.unique_cmp @@ ius @ pred_ius in
      let iu_idx_map =
        let tbl : (Table.Iu.t, int) Hashtbl.t =
          Hashtbl.create @@ List.length ius
        in
        let find_idx iu =
          match List.index_of iu ius with
          | Some i ->
              Hashtbl.add tbl iu i
          | None ->
              ()
        in
        List.iter find_idx ius ;
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
      let tuple = Table.Iter.next tbl_scan_ctx.iter in
      Option.map (Storage.Tuple.extract_values tbl_scan_ctx.attr_idxs) tuple
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
  | Projection (ius, child) ->
      Option.map (Storage.Tuple.take @@ List.length ius) @@ next ctx child
