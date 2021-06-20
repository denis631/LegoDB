open Frontend
open BatteriesExceptionless

let read () = Sql_parser.parse @@ read_line ()

let eval query =
  Printf.printf "%s\n---\n" @@ Ast.show query ;
  Query.Driver.run Bootstrapper.db query


let print = print_endline % Storage.Tuple.show

let rec loop () =
  match read () with
  | Ok query ->
      Query.Driver.benchmark (fun () -> eval query print) ;
      loop ()
  | Error e ->
      print_endline e ;
      loop ()
