open Storage

let rec optimize = function
  | Logical.Operators.TableScan tbl -> Physical.Table_scan.make (Table.Iter.make tbl) (Table.ius tbl)
  | Logical.Operators.Selection (match_tree, op) -> Physical.Selection.make match_tree (optimize op)
  | Logical.Operators.Projection (proj_attrs, op) -> Physical.Projection.make proj_attrs (optimize op)
  | Logical.Operators.CrossProduct (_, _) -> failwith "TODO: no real cross products, only joins"
