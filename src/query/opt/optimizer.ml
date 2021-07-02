open Storage

let rec optimize = function
  | Logical.Operators.TableScan tbl -> Physical.Operators.TableScan { iter = Table.Iter.make tbl; ius = Table.ius tbl; }
  | Logical.Operators.Selection (match_tree, op) -> Physical.Operators.Selection (match_tree, optimize op)
  | Logical.Operators.Projection (proj_attrs, op) -> Physical.Operators.Projection (proj_attrs, optimize op)
  | Logical.Operators.CrossProduct (_, _) -> failwith "TODO: no real cross products, only joins"
