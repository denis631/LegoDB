open Core
open Utils

let find_column_attr tbl_meta_lst attr_name =
  let find_attr_for_tbl (tbl_meta : TableMeta.t) =
    let filter_col (iu : Schema.Iu.t) = String.equal iu.column attr_name in
    List.find ~f:filter_col tbl_meta.schema
  in
  match List.filter_map ~f:find_attr_for_tbl tbl_meta_lst with
  | [ x ] -> x
  | _ :: _ ->
      failwith @@ "There are multiple tables with the attribute " ^ attr_name
  | [] -> failwith @@ "Attribute with name " ^ attr_name ^ " not found"
