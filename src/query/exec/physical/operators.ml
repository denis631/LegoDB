open BatteriesExceptionless

type exec_ctx = unit

(* | InnerJoin of match_expr_annot * operator * operator *)

let rec prepare ius = function
  | Table_scan.TableScan tbl_scan ->
    Table_scan.TableScan (Table_scan.prepare ius tbl_scan)
  | Selection.Selection selection ->
    Selection.make selection.predicate (prepare ius selection.childOp)
  | Projection.Projection projection ->
    Projection.make projection.attributes (prepare ius projection.childOp)
  | _ -> failwith "unhandled case"


let rec next ctx = function
  | Table_scan.TableScan tbl_scan ->
    Table_scan.next ctx tbl_scan
  | Selection.Selection selection ->
    Selection.next next ctx selection
  | Projection.Projection projection ->
    Projection.next next ctx projection
  | _ -> failwith "unhandled case"
