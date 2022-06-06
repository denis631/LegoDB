{
open Parser

exception SyntaxError of string

let[@inline] failwith msg = raise (SyntaxError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+  (* regex for integers *)
let id = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule next_token = parse
  | eof { EOF }
  | whitespace+ { next_token lexbuf }
  | newline     { Lexing.new_line lexbuf; next_token lexbuf }

  (* YOUR TOKENS HERE... *)
  | int         { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ';'         { SEMICOLON_SYMBOL }
  | ','         { COMMA_SYMBOL }
  | '='         { EQ_SYMBOL }
  | '*'         { STAR_SYMBOL }
  | "select"    { SELECT_SYMBOL }
  | "where"     { WHERE_SYMBOL }
  | "and"       { AND_SYMBOL }
  | "from"      { FROM_SYMBOL }
  | "create"    { CREATE_SYMBOL }
  | "table"     { TABLE_SYMBOL }
  | "primary"   { PRIMARY_SYMBOL }
  | "key"       { KEY_SYMBOL }
  | "copy"      { COPY_SYMBOL }
  | '('         { LPAR_SYMBOL }
  | ')'         { RPAR_SYMBOL }
  | "int"       { INT_SYMBOL }
  | "numeric"   { NUMERIC_SYMBOL }
  | "char"      { CHAR_SYMBOL }
  | "varchar"   { VARCHAR_SYMBOL }
  | "timestamp" { TIMESTAMP_SYMBOL }
  | id          { ID (Lexing.lexeme lexbuf) }
  | '"'         { read_string (Buffer.create 36) lexbuf }

  (* no match? raise exception *)
  | _ as c { illegal c }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
