open Storage

(* let rec optimize db = function *)
(*   | Logical.Operators.TableScan tbl -> *)
(*       Physical.Table_scan.make ~meta:tbl ~ius:(Table.ius tbl) *)
(*   | Logical.Operators.Selection *)
(*       ( Expr.Match.Expr.Eq *)
(*           ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs), *)
(*             Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ), *)
(*         Logical.Operators.CrossProduct (left_op, right_op) ) -> *)
(*       let opt_left_op = optimize db left_op in *)
(*       let opt_right_op = optimize db right_op in *)
(*       let key_ius = *)
(*         if Physical.Operators.has_iu lhs opt_left_op then ([ lhs ], [ rhs ]) *)
(*         else ([ rhs ], [ lhs ]) *)
(*       in *)
(*       Physical.Hash_join.make ~left_op:opt_left_op ~right_op:opt_right_op *)
(*         ~hash_key_ius:key_ius *)
(*   | Logical.Operators.Selection (match_tree, op) -> ( *)
(*       let child_op = optimize db op in *)
(*       match (match_tree, child_op) with *)
(*       | ( Expr.Match.Expr.Eq *)
(*             ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs), *)
(*               Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ), *)
(*           Physical.Hash_join.HashJoin join ) -> *)
(*           Physical.Hash_join.make ~left_op:join.left_op ~right_op:join.right_op *)
(*             ~hash_key_ius: *)
(*               (lhs :: fst join.hash_key_ius, rhs :: snd join.hash_key_ius) *)
(*       | ( Expr.Match.Expr.Eq *)
(*             ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr iu), *)
(*               Expr.Match.Expr.Leaf (Expr.Match.Expr.Const _) ), *)
(*           _ ) -> *)
(*           predicate_pushdown iu *)
(*             (Physical.Selection.make ~predicate:match_tree ~child_op) *)
(*       | _ -> Physical.Selection.make ~predicate:match_tree ~child_op) *)
(*   | Logical.Operators.Projection (proj_attrs, op) -> *)
(*       Physical.Projection.make ~attributes:proj_attrs ~child_op:(optimize db op) *)
(*   | Logical.Operators.CrossProduct (_, _) -> *)
(*       failwith *)
(*         "TODO: no real cross products are allowed to be produced, only joins" *)
(*   | Logical.Operators.Copy (tbl_name, path) -> *)
(*       let tbl_meta = Catalog.find_table (Database.catalog db) tbl_name in *)
(*       let row_parser = *)
(*         Physical.Parse_row_file.make ~path *)
(*           ~schema:(Table.Meta.schema tbl_meta) *)
(*       in *)
(*       Physical.Bulk_insert.make ~child_op:row_parser ~meta:tbl_meta *)
(*   | Logical.Operators.CreateTbl tbl_meta -> Physical.Create_tbl.make ~tbl_meta *)
(*   | Logical.Operators.DropTbl tbl_metas -> Physical.Drop_tbl.make ~tbl_metas *)

let fs = Physical.Operators.funcs

let rec to_stages db = function
  | Logical.Operators.TableScan tbl ->
      Physical.Table_scan.make ~meta:tbl
  | Logical.Operators.Selection (expr, op) ->
      Physical.Selection.make fs ~predicate:expr ~child_op:(to_stages db op)
  | Logical.Operators.Projection (proj_attrs, op) ->
      Physical.Projection.make fs ~schema:proj_attrs
        ~child_op:(to_stages db op)
  | Logical.Operators.CrossProduct (_, _) ->
      failwith
        "TODO: no real cross products are allowed to be produced, only joins"
  | Logical.Operators.Join (left_op, right_op, ius) ->
      Physical.Hash_join.make ~left_op:(to_stages db left_op)
        ~right_op:(to_stages db right_op) ~hash_key_ius:ius
  | Logical.Operators.Copy (tbl_name, path) ->
      let tbl_meta = Catalog.find_table (Database.catalog db) tbl_name in
      let row_parser =
        Physical.Parse_row_file.make ~path ~schema:tbl_meta.schema
      in
      Physical.Bulk_insert.make ~child_op:row_parser ~meta:tbl_meta
  | Logical.Operators.CreateTbl tbl_meta -> Physical.Create_tbl.make ~tbl_meta
  | Logical.Operators.DropTbl tbl_metas -> Physical.Drop_tbl.make ~tbl_metas

let optimize = to_stages
