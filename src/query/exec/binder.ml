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


let find_column_attr (db : Database.t) attr_name =
  let find_attr_for_tbl tbl =
    let filter_col (col, ty) =
      if col = attr_name then Some (Table.(tbl.name), attr_name, ty) else None
    in
    match List.filter_map filter_col Table.(tbl.schema) with
    | [ x ] ->
        Some x
    | _ ->
        None
  in
  match List.find_map find_attr_for_tbl db.tables with
  | Some x ->
      x
  | None ->
      failwith @@ "Attribute with name " ^ attr_name ^ " not found"
