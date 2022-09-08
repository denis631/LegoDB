(* let rec optimize db = function *)
(*   | Planner.Operators.TableScan tbl -> *)
(*       Executor.Table_scan.make ~meta:tbl ~ius:(Table.ius tbl) *)
(*   | Planner.Operators.Selection *)
(*       ( Expr.Match.Expr.Eq *)
(*           ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs), *)
(*             Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ), *)
(*         Planner.Operators.CrossProduct (left_op, right_op) ) -> *)
(*       let opt_left_op = optimize db left_op in *)
(*       let opt_right_op = optimize db right_op in *)
(*       let key_ius = *)
(*         if Executor.Operators.has_iu lhs opt_left_op then ([ lhs ], [ rhs ]) *)
(*         else ([ rhs ], [ lhs ]) *)
(*       in *)
(*       Executor.Hash_join.make ~left_op:opt_left_op ~right_op:opt_right_op *)
(*         ~hash_key_ius:key_ius *)
(*   | Planner.Operators.Selection (match_tree, op) -> ( *)
(*       let child_op = optimize db op in *)
(*       match (match_tree, child_op) with *)
(*       | ( Expr.Match.Expr.Eq *)
(*             ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs), *)
(*               Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ), *)
(*           Executor.Hash_join.HashJoin join ) -> *)
(*           Executor.Hash_join.make ~left_op:join.left_op ~right_op:join.right_op *)
(*             ~hash_key_ius: *)
(*               (lhs :: fst join.hash_key_ius, rhs :: snd join.hash_key_ius) *)
(*       | ( Expr.Match.Expr.Eq *)
(*             ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr iu), *)
(*               Expr.Match.Expr.Leaf (Expr.Match.Expr.Const _) ), *)
(*           _ ) -> *)
(*           predicate_pushdown iu *)
(*             (Executor.Selection.make ~predicate:match_tree ~child_op) *)
(*       | _ -> Executor.Selection.make ~predicate:match_tree ~child_op) *)
(*   | Planner.Operators.Projection (proj_attrs, op) -> *)
(*       Executor.Projection.make ~attributes:proj_attrs ~child_op:(optimize db op) *)
(*   | Planner.Operators.CrossProduct (_, _) -> *)
(*       failwith *)
(*         "TODO: no real cross products are allowed to be produced, only joins" *)
(*   | Planner.Operators.Copy (tbl_name, path) -> *)
(*       let tbl_meta = Catalog.find_table (Database.catalog db) tbl_name in *)
(*       let row_parser = *)
(*         Executor.Parse_row_file.make ~path *)
(*           ~schema:(TableMeta.schema tbl_meta) *)
(*       in *)
(*       Executor.Bulk_insert.make ~child_op:row_parser ~meta:tbl_meta *)
(*   | Planner.Operators.CreateTbl tbl_meta -> Executor.Create_tbl.make ~tbl_meta *)
(*   | Planner.Operators.DropTbl tbl_metas -> Executor.Drop_tbl.make ~tbl_metas *)

let fs = Executor.Operators.funcs

let rec to_stages catalog = function
  | Planner.Operators.TableScan tbl -> Executor.Table_scan.make ~meta:tbl
  | Planner.Operators.Selection (op, expr) ->
      Executor.Selection.make fs ~predicate:expr
        ~child_op:(to_stages catalog op)
  | Planner.Operators.Projection (op, Planner.Operators.All) ->
      to_stages catalog op
  | Planner.Operators.Projection (op, Planner.Operators.Attributes proj_attrs)
    ->
      Executor.Projection.make fs ~schema:proj_attrs
        ~child_op:(to_stages catalog op)
  | Planner.Operators.Join (left_op, right_op, ius, _) ->
      Executor.Hash_join.make
        ~left_op:(to_stages catalog left_op)
        ~right_op:(to_stages catalog right_op)
        ~hash_key_ius:ius
  | Planner.Operators.Copy (tbl_name, path) ->
      let tbl_meta = Catalog.find_tbl catalog tbl_name in
      let row_parser =
        Executor.File_record_parser.make ~path ~schema:tbl_meta.schema
      in
      Executor.Inserter.make ~child_op:row_parser ~meta:tbl_meta
        ~is_bulk_insert:true
  | Planner.Operators.CreateTbl tbl_meta -> Executor.Create_tbl.make ~tbl_meta
  | Planner.Operators.DropTbl tbl_metas ->
      Executor.Drop_tbl.make ~metas:tbl_metas
  | Planner.Operators.Limit (child_op, limit) ->
      Executor.Limit.make ~child_op:(to_stages catalog child_op) ~limit

let optimize = to_stages
