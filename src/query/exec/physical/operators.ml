type exec_ctx = unit

(* NOTE: what about this function? *)
let rec has_iu iu = function
  | Table_scan.TableScan tbl_scan -> Table_scan.has_iu has_iu iu tbl_scan
  | Selection.Selection selection -> Selection.has_iu has_iu iu selection
  | Projection.Projection projection -> Projection.has_iu has_iu iu projection
  | Hash_join.HashJoin join -> Hash_join.has_iu has_iu iu join
  | _ -> failwith "unhandled case"

let rec open_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.open_op tbl_scan
  | Selection.Selection selection -> Selection.open_op funcs selection
  | Projection.Projection projection -> Projection.open_op funcs projection
  | Hash_join.HashJoin join -> Hash_join.open_op funcs join
  | _ -> failwith "unhandled case"

and close_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.close_op tbl_scan
  | Selection.Selection selection -> Selection.close_op funcs selection
  | Projection.Projection projection -> Projection.close_op funcs projection
  | Hash_join.HashJoin join -> Hash_join.close_op funcs join
  | _ -> failwith "unhandled case"

and next ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.next ctx tbl_scan
  | Selection.Selection selection -> Selection.next funcs ctx selection
  | Projection.Projection projection -> Projection.next funcs ctx projection
  | Hash_join.HashJoin join -> Hash_join.next funcs ctx join
  | _ -> failwith "unhandled case"

and funcs = { open_op; close_op; next }
