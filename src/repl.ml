open Frontend

let read () = Sql_parser.parse @@ read_line ()

let eval query =
  Printf.printf "%s\n---\n" @@ Ast.show query ;
  Exec.Runner.run Bootstrapper.db query


let print = List.iter (fun t -> print_endline @@ Storage.Tuple.show t)

let rec loop () =
  match read () with
  | Ok query ->
      Exec.Runner.benchmark (fun () -> print @@ eval query) ;
      loop ()
  | Error e ->
      print_endline e ;
      loop ()
