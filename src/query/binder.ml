open Storage

let find_column_attr db attr_name =
  let find_attr_for_tbl tbl =
    let filter_col (col, ty) =
      if col = attr_name then
        Some (Table.T.Iu.make (Table.T.Meta.name tbl) attr_name ty)
      else None
    in
    match List.filter_map filter_col @@ fst @@ Table.T.Meta.schema tbl with
    | [ x ] -> Some x
    | _ -> None
  in
  match
    List.find_map find_attr_for_tbl @@ (db |> Database.catalog |> Catalog.tbls)
  with
  | Some x -> x
  | None -> failwith @@ "Attribute with name " ^ attr_name ^ " not found"
