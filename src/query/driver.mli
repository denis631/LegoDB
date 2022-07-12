open Storage
open Frontend.Ast
open Expr

val make_match_tree : Table.Meta.t list -> pred list -> Match.Expr.boolean
val make_operator_tree : Database.t -> sql_expr -> Logical.Operators.t
val run : Database.t -> sql_expr -> (string -> unit) -> unit
val benchmark : (unit -> 'a) -> 'a
