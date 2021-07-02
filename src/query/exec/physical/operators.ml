open Storage
open Expr
open BatteriesExceptionless

type exec_ctx = unit

type tbl_scan_ctx =
  { iter : Table.Iter.t
  ; ius : Table.Iu.t list
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
let rec prepare _ = function
  | TableScan tbl_scan_ctx ->
      TableScan { iter = tbl_scan_ctx.iter; ius = tbl_scan_ctx.ius }
  | Selection (expr, op) ->
      Selection (expr, op)
  | Projection (ius, op) ->
      Projection (ius, prepare ius op)


let rec next ctx = function
  | TableScan tbl_scan_ctx ->
      tbl_scan_ctx.iter
      |> Table.Iter.next
      |> Option.map (fun t -> (t, tbl_scan_ctx.ius))
      (* TODO: uncomment this later as an extra optimization,
       *       fetching only needed attributes from the start *)
      (* Option.map (Storage.Tuple.extract_values tbl_scan_ctx.attr_idxs) tuple *)
  | Selection (expr, child) ->
      let rec probe () =
        match next ctx child with
        | Some x ->
            let is_true = Value.eq @@ Value.Bool true in
            if is_true @@ Match.Expr.eval x @@ Match.Expr.BoolExpr expr
            then Some x
            else probe ()
        | None ->
            None
      in
      probe ()
  | Projection (ius, child) ->
    (* TODO: do not take just first k attributes, but rather according to the set of attributes *)
    (* Option.map (Storage.Tuple.take @@ List.length ius) @@ next ctx child *)
    ( match next ctx child with
    | Some (tuple, schema) ->
        let should_project_iu iu = List.exists (Table.Iu.eq iu) ius in
        let zip = List.map2 (fun x y -> (x, y)) in
        let unzip coll =
          let xs = List.map fst coll in
          let ys = List.map snd coll in
          (xs, ys)
        in
        let new_tuple, new_schema =
          zip tuple schema |> List.filter (should_project_iu % snd) |> unzip
        in
        Some (new_tuple, new_schema)
    | None ->
        None )
