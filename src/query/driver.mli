open Storage
open Frontend.Ast
open Expr

val make_match_tree : Table.T.Meta.t list -> pred list -> Match.Expr.bool
val make_operator_tree : Database.t -> sql_expr -> Logical.Operators.t
val run : Database.t -> sql_expr -> (Tuple.t -> unit) -> unit
val benchmark : (unit -> 'a) -> 'a
