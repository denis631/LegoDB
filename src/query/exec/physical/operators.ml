open BatteriesExceptionless

type exec_ctx = unit


let rec prepare ius = function
  | Table_scan.TableScan tbl_scan ->
      Table_scan.TableScan (Table_scan.prepare ius tbl_scan)
  | Selection.Selection selection ->
      Selection.make
        ~predicate:selection.predicate
        ~childOp:(prepare ius selection.childOp)
  | Projection.Projection projection ->
      Projection.make
        ~attributes:projection.attributes
        ~childOp:(prepare ius projection.childOp)
  | Hash_join.HashJoin join ->
    Hash_join.make
      ~leftOp:(prepare ius join.leftOp)
      ~rightOp:(prepare ius join.rightOp)
      ~hash_key_ius:join.hash_key_ius
  | _ ->
      failwith "unhandled case"


let rec next ctx = function
  | Table_scan.TableScan tbl_scan ->
      Table_scan.next ctx tbl_scan
  | Selection.Selection selection ->
      Selection.next next ctx selection
  | Projection.Projection projection ->
      Projection.next next ctx projection
  | Hash_join.HashJoin join ->
      Hash_join.next next ctx join
  | _ ->
      failwith "unhandled case"
