open Frontend

let read () = Sql_parser.parse @@ read_line ()

let eval query =
  print_endline @@ Ast.show query;
  print_endline "---";
  [] (* TODO: implement query evaluator *)

let print = List.iter print_endline

let rec loop () =
  match read () with
  | Ok query ->
    print @@ eval query;
    loop ()
  | Error e ->
    print @@ [e];
    loop ()

let run = loop
