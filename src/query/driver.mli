open Frontend.Ast

val make_operator_tree : Catalog.t -> sql_expr -> Logical.Operators.t
val run : Catalog.t -> sql_expr -> (string -> unit) -> unit
val benchmark : (unit -> 'a) -> 'a
