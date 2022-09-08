let rec output_schema = function
  | Table_scan.TableScan tbl_scan -> tbl_scan.meta.schema
  | Selection.Selection selection -> output_schema selection.child_op
  | Projection.Projection projection -> projection.schema
  | Hash_join.HashJoin _ -> failwith "to implement"
  | Limit.Limit limit -> output_schema limit.child_op
  | _ -> []

(* NOTE: what about this function? *)
let rec has_iu iu = function
  | Table_scan.TableScan tbl_scan -> Table_scan.has_iu has_iu iu tbl_scan
  | Selection.Selection selection -> Selection.has_iu has_iu iu selection
  | Projection.Projection projection -> Projection.has_iu has_iu iu projection
  | Hash_join.HashJoin join -> Hash_join.has_iu has_iu iu join
  | Limit.Limit limit -> Limit.has_iu has_iu iu limit
  | _ -> false

let rec open_op ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.open_op funcs ctx tbl_scan
  | Selection.Selection selection -> Selection.open_op funcs ctx selection
  | Projection.Projection projection -> Projection.open_op funcs ctx projection
  | Hash_join.HashJoin join -> Hash_join.open_op funcs ctx join
  | Inserter.Inserter inserter -> Inserter.open_op funcs ctx inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.open_op funcs ctx parser
  | Create_tbl.CreateTbl creater -> Create_tbl.open_op funcs ctx creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.open_op funcs ctx dropper
  | Limit.Limit limit -> Limit.open_op funcs ctx limit
  | _ -> failwith "unhandled case"

and close_op ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.close_op funcs ctx tbl_scan
  | Selection.Selection selection -> Selection.close_op funcs ctx selection
  | Projection.Projection projection -> Projection.close_op funcs ctx projection
  | Hash_join.HashJoin join -> Hash_join.close_op funcs ctx join
  | Inserter.Inserter inserter -> Inserter.close_op funcs ctx inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.close_op funcs ctx parser
  | Create_tbl.CreateTbl creater -> Create_tbl.close_op funcs ctx creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.close_op funcs ctx dropper
  | Limit.Limit limit -> Limit.close_op funcs ctx limit
  | _ -> failwith "unhandled case"

and next ctx = function
  | Table_scan.TableScan tbl_scan -> Table_scan.next funcs ctx tbl_scan
  | Selection.Selection selection -> Selection.next funcs ctx selection
  | Projection.Projection projection -> Projection.next funcs ctx projection
  | Hash_join.HashJoin join -> Hash_join.next funcs ctx join
  | Inserter.Inserter inserter -> Inserter.next funcs ctx inserter
  | File_record_parser.RecordParser parser ->
      File_record_parser.next funcs ctx parser
  | Create_tbl.CreateTbl creater -> Create_tbl.next funcs ctx creater
  | Drop_tbl.DropTbl dropper -> Drop_tbl.next funcs ctx dropper
  | Limit.Limit limit -> Limit.next funcs ctx limit
  | _ -> failwith "unhandled case"

and funcs = { output_schema; open_op; close_op; next }
