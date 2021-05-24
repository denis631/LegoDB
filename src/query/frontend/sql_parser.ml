open Ast

type parse_error = String.t
type parse_result = (sql_expr, parse_error) result

let parse (s : string) : parse_result =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.query Lexer.next_token lexbuf in
    Ok ast
  with
  | Lexer.SyntaxError s ->
    Error s
  | _ ->
    Error "unknown error while parsing"
