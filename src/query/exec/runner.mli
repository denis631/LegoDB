open Storage
open Frontend.Ast

val make_match_tree : Database.t -> pred list -> Match.Expr.bool

val make_operator_tree : Database.t -> sql_expr -> Operators.operator

val run : Database.t -> sql_expr -> Tuple.t list

val benchmark : (unit -> 'a) -> 'a
