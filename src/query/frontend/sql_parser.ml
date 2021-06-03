open Ast

type parse_error = string

type parse_result = (sql_expr, parse_error) result

let parse (s : string) : parse_result =
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.query Lexer.next_token lexbuf) with
  | Lexer.SyntaxError s ->
      Error s
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let error_meta =
        string_of_int pos.pos_lnum
        ^ ":"
        ^ string_of_int (pos.pos_cnum - pos.pos_bol + 1)
      in
      Error ("Parse error: " ^ error_meta)
