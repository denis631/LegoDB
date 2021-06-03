open Frontend

let read () = Sql_parser.parse @@ read_line ()

let eval query =
  print_endline @@ Ast.show query ;
  print_endline "---" ;
  Exec.Runner.run Bootstrapper.db query


let print = List.iter (fun t -> print_endline @@ Storage.Tuple.show t)

let rec loop () =
  match read () with
  | Ok query ->
      print @@ eval query ;
      loop ()
  | Error e ->
      print_endline e ;
      loop ()


let run = loop
