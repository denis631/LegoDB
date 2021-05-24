(* open Lego_db *)

let _ =
  let query = Frontend.Sql_parser.parse "select x,y from table1, table2 where x = 10;" in
  match query with
  | Ok s -> print_endline @@ Frontend.Ast.show s
  | Error e -> print_endline e
(* Bootstrapper.bootstrap () *)
