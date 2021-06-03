open Frontend.Ast
open Storage

let find_table (db : Database.t) tbl_name =
  try
    List.find
      (fun tbl -> match tbl_name with TblName x -> Table.(tbl.name) = x)
      db.tables
  with
  | _ ->
      let show_tbl = function Frontend.Ast.TblName s -> s in
      failwith @@ "Table with name " ^ show_tbl tbl_name ^ " not found"


let find_column_attr _ _ = failwith "TODO: implement"
