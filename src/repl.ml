open Frontend

let read () = Sql_parser.parse @@ read_line ()

let eval query =
  Printf.printf "%s\n---\n" @@ Ast.show_sql_expr query;
  Query.Driver.run Catalog.instance query

let print = print_endline

let rec loop () =
  match read () with
  | Ok query ->
      Query.Driver.benchmark (fun () -> eval query print);
      loop ()
  | Error e ->
      print_endline e;
      loop ()
