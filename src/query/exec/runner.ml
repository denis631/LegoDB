open Frontend.Ast
open Operators

let make_operator_tree db = function
  (* | Select (attr_lst, tbl_lst, pred_lst) -> *)
  | Select (attr_lst, tbl_lst, _) ->
      let tbls = List.map (Binder.find_table db) tbl_lst in
      let tbl_scan =
        let tbl = List.hd tbls in
        TableScan { tbl; attr_idxs = []; tuples = tbl.tuples }
      in
      let attrs =
        if List.exists (( = ) Star) attr_lst
        then failwith "TODO: implement projection of all attributes"
        else
          attr_lst
          |> List.filter_map (fun attr ->
                 match attr with AttrName s -> Some s | Star -> None )
          |> List.map (Binder.find_column_attr db)
      in
      Projection (attrs, tbl_scan)


(* Projection (attr_lst, Selection (pred_lst, TableScan tbl_lst)) *)

let run db ast =
  let tree = prepare [] @@ make_operator_tree db ast in
  let ctx = () in
  let rec exec acc =
    match next ctx tree with Some t -> exec (t :: acc) | None -> acc
  in
  exec []
