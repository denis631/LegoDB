type parse_error = string

type parse_result = (Ast.sql_expr, parse_error) result

val parse : string -> parse_result
