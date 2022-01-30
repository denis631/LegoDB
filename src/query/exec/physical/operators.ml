open BatteriesExceptionless

type exec_ctx = unit

let rec has_iu iu = function
  | Table_scan.TableScan tbl_scan ->
      Table_scan.has_iu has_iu iu tbl_scan
  | Selection.Selection selection ->
      Selection.has_iu has_iu iu selection
  | Projection.Projection projection ->
      Projection.has_iu has_iu iu projection
  | Hash_join.HashJoin join ->
      Hash_join.has_iu has_iu iu join
  | _ ->
      failwith "unhandled case"

(* FIXME: prepare functions on ops are not called *)
let rec prepare ius = function
  | Table_scan.TableScan tbl_scan ->
      Table_scan.TableScan (Table_scan.prepare ius tbl_scan)
  | Selection.Selection selection ->
      Selection.make
        ~predicate:selection.predicate
        ~child_op:(prepare ius selection.child_op)
  | Projection.Projection projection ->
      Projection.make
        ~attributes:projection.attributes
        ~child_op:(prepare ius projection.child_op)
  | Hash_join.HashJoin join ->
    Hash_join.make
      ~left_op:(prepare ius join.left_op)
      ~right_op:(prepare ius join.right_op)
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
