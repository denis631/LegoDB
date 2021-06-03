open Frontend.Ast
open Operators

let make_operator_tree db = function
  (* | Select (attr_lst, tbl_lst, pred_lst) -> *)
  | Select (_, tbl_lst, _) ->
      let tbls = List.map (Binder.find_table db) tbl_lst in
      (* TODO: add binding to the existing column names *)
      TableScan { tuples = (List.hd tbls).tuples }


(* Projection (attr_lst, Selection (pred_lst, TableScan tbl_lst)) *)

let run db ast =
  let tree = make_operator_tree db ast in
  let ctx = () in
  let rec exec acc =
    match next ctx tree with Some t -> exec (t :: acc) | None -> acc
  in
  exec []
