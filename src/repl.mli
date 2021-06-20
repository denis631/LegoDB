val read : unit -> Frontend.Sql_parser.parse_result

val eval : Frontend.Ast.sql_expr -> (Storage.Tuple.t -> unit) -> unit

val print : Storage.Tuple.t -> unit

val loop : unit -> unit
