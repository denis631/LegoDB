type exec_ctx = unit

let rec output_schema = function
  | Table_scan.TableScan tbl_scan -> tbl_scan.meta.schema
  | Selection.Selection selection -> output_schema selection.child_op
  | Projection.Projection projection -> projection.schema
  | Hash_join.HashJoin _ -> failwith "to implement"
  | _ -> []

(* NOTE: what about this function? *)
let rec has_iu iu = function
  | Table_scan.TableScan tbl_scan -> Table_scan.has_iu has_iu iu tbl_scan
  | Selection.Selection selection -> Selection.has_iu has_iu iu selection
  | Projection.Projection projection -> Projection.has_iu has_iu iu projection
  | Hash_join.HashJoin join -> Hash_join.has_iu has_iu iu join
  | _ -> false

let rec open_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.open_op tbl_scan
  | Selection.Selection selection -> Selection.open_op funcs selection
  | Projection.Projection projection -> Projection.open_op funcs projection
  | Hash_join.HashJoin join -> Hash_join.open_op funcs join
  | Bulk_insert.BulkInserter inserter -> Bulk_insert.open_op funcs inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.open_op funcs parser
  | Create_tbl.CreateTbl creater -> Create_tbl.open_op funcs creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.open_op funcs dropper
  | _ -> failwith "unhandled case"

and close_op = function
  | Table_scan.TableScan tbl_scan -> Table_scan.close_op tbl_scan
  | Selection.Selection selection -> Selection.close_op funcs selection
  | Projection.Projection projection -> Projection.close_op funcs projection
  | Hash_join.HashJoin join -> Hash_join.close_op funcs join
  | Bulk_insert.BulkInserter inserter -> Bulk_insert.close_op funcs inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.close_op funcs parser
  | Create_tbl.CreateTbl creater -> Create_tbl.close_op funcs creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.close_op funcs dropper
  | _ -> failwith "unhandled case"

and next ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.next ctx tbl_scan
  | Selection.Selection selection -> Selection.next funcs ctx selection
  | Projection.Projection projection -> Projection.next funcs ctx projection
  | Hash_join.HashJoin join -> Hash_join.next funcs ctx join
  | Bulk_insert.BulkInserter inserter -> Bulk_insert.next funcs ctx inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.next funcs ctx parser
  | Create_tbl.CreateTbl creater -> Create_tbl.next funcs ctx creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.next funcs ctx dropper
  | _ -> failwith "unhandled case"

and funcs = { output_schema; open_op; close_op; next }
