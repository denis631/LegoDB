val make_operator_tree : Catalog.t -> Binder.Ast.sql_expr -> Planner.Node.t
val run : Legodb.t -> Frontend.Ast.sql_expr -> (string -> unit) -> unit
val benchmark : (unit -> 'a) -> 'a
