open Storage
open Core

let find_column_attr tbl_meta_lst attr_name =
  let find_attr_for_tbl tbl_meta =
    let filter_col (col, ty) =
      Option.some_if
        (String.equal col attr_name)
        (Table.T.Iu.make (Table.T.Meta.name tbl_meta) attr_name ty)
    in
    List.find_map ~f:filter_col @@ Table.T.Meta.schema tbl_meta
  in
  match List.filter_map ~f:find_attr_for_tbl tbl_meta_lst with
  | [ x ] -> x
  | _ :: _ ->
      failwith @@ "There are multiple tables with the attribute " ^ attr_name
  | [] -> failwith @@ "Attribute with name " ^ attr_name ^ " not found"
