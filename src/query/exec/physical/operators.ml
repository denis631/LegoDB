open BatteriesExceptionless

type exec_ctx = unit

let rec has_iu iu = function
  | Table_scan.TableScan tbl_scan -> Table_scan.has_iu has_iu iu tbl_scan
  | Selection.Selection selection -> Selection.has_iu has_iu iu selection
  | Projection.Projection projection -> Projection.has_iu has_iu iu projection
  | Hash_join.HashJoin join -> Hash_join.has_iu has_iu iu join
  | _ -> failwith "unhandled case"

let rec open_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.open_op tbl_scan
  | Selection.Selection selection -> Selection.open_op open_op selection
  | Projection.Projection projection -> Projection.open_op open_op projection
  | Hash_join.HashJoin join -> Hash_join.open_op open_op join
  | _ -> failwith "unhandled case"

let rec close_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.close_op tbl_scan
  | Selection.Selection selection -> Selection.close_op close_op selection
  | Projection.Projection projection -> Projection.close_op close_op projection
  | Hash_join.HashJoin join -> Hash_join.close_op close_op join
  | _ -> failwith "unhandled case"

let rec next ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.next ctx tbl_scan
  | Selection.Selection selection -> Selection.next next ctx selection
  | Projection.Projection projection -> Projection.next next ctx projection
  | Hash_join.HashJoin join -> Hash_join.next next ctx join
  | _ -> failwith "unhandled case"
