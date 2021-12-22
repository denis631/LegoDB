open Storage

let rec optimize = function
  | Logical.Operators.TableScan tbl ->
      Physical.Table_scan.make ~iter:(Table.Iter.make tbl) ~ius:(Table.ius tbl)
  | Logical.Operators.Selection (match_tree, op) ->
      Physical.Selection.make ~predicate:match_tree ~childOp:(optimize op)
  | Logical.Operators.Projection (proj_attrs, op) ->
      Physical.Projection.make ~attributes:proj_attrs ~childOp:(optimize op)
  | Logical.Operators.CrossProduct (_, _) ->
      failwith "TODO: no real cross products, only joins"
