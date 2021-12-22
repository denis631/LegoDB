open Frontend.Ast
open Storage

let find_table db tbl_name =
  try
    db
    |> Database.tbls
    |> List.find (fun tbl ->
           match tbl_name with TblName x -> Table.name tbl = x )
  with
  | _ ->
      let show_tbl = function Frontend.Ast.TblName s -> s in
      failwith @@ "Table with name " ^ show_tbl tbl_name ^ " not found"


let find_column_attr db attr_name =
  let find_attr_for_tbl tbl =
    let filter_col (col, ty) =
      if col = attr_name
      then Some (Table.Iu.make (Table.name tbl) attr_name ty)
      else None
    in
    match List.filter_map filter_col @@ Table.schema tbl with
    | [ x ] ->
        Some x
    | _ ->
        None
  in
  match List.find_map find_attr_for_tbl @@ Database.tbls db with
  | Some x ->
      x
  | None ->
      failwith @@ "Attribute with name " ^ attr_name ^ " not found"