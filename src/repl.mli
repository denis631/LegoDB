val read : unit -> Frontend.Sql_parser.parse_result

val eval : Frontend.Ast.sql_expr -> (string -> unit) -> unit

val print : string -> unit

val loop : unit -> unit
