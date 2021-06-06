open Storage
open BatteriesExceptionless

type exec_ctx = unit

(* TODO: provide an abstraction (data cursor) that delivers data to the operators *)
type tbl_scan_ctx =
  { tbl : Table.t
  ; mutable tuples : Storage.Tuple.t list
  ; attr_idxs : int list
  }

type proj_attrs = Table.iu list

type operator =
  | TableScan of tbl_scan_ctx (* TODO: this is too low level. *)
  | Selection of Match.bool_expr * operator
  | Projection of proj_attrs * operator

(* | CrossProduct of operator * operator *)
(* TODO: should be created by joining predicate and cross product *)
(* | InnerJoin of match_expr_annot * operator * operator *)

(* TODO: for pushing the attributes to read *)
let rec prepare ius = function
  | TableScan tbl_scan_ctx ->
      let get_idx idx (c1, ty1) =
        if List.exists
             (fun (tbl_name, c2, ty2) ->
               tbl_scan_ctx.tbl.name = tbl_name && c1 = c2 && ty1 = ty2 )
             ius
        then Some idx
        else None
      in
      let attr_idxs =
        List.filteri_map get_idx Table.(tbl_scan_ctx.tbl.schema)
      in
      TableScan
        { tbl = tbl_scan_ctx.tbl; tuples = tbl_scan_ctx.tuples; attr_idxs }
  | Selection (expr, op) ->
      let pred_ius = Match.ius_of_bool_expr expr in
      let ius = List.unique @@ ius @ pred_ius in
      let iu_idx_map =
        let tbl : (Table.iu, int) Hashtbl.t =
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
      Selection (Match.prepare_bool_expr iu_idx_map expr, prepare ius op)
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
            if Match.eval_bool_expr tuple expr then Some tuple else probe ()
        | None ->
            None
      in
      probe ()
  | Projection (_, child) ->
      next ctx child
