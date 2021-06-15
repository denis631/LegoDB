val read : unit -> Frontend.Sql_parser.parse_result

val eval : Frontend.Ast.sql_expr -> Storage.Tuple.t list

val print : Storage.Tuple.t list -> unit

val loop : unit -> unit
