open Frontend.Ast
open Operators
open Storage

let make_match_tree db pred_lst =
  let make_expr pred =
    let make_attr_iu = function
      | AttrName x ->
          Binder.find_column_attr db x
      | _ ->
          failwith "TODO: support this use-case"
    in
    let make_const = function
      | Int i ->
          Match.Expr.Leaf (Match.Expr.Const (Value.Integer i))
      | Str s ->
          Match.Expr.Leaf (Match.Expr.Const (Value.StringLiteral s))
    in
    match pred with
    | EqConst (attr, const) ->
        let lhs = Match.Expr.Leaf (Match.Expr.TableAttr (make_attr_iu attr)) in
        let rhs = make_const const in
        Match.Expr.Eq (lhs, rhs)
    | EqAttr (attr1, attr2) ->
        let lhs = Match.Expr.Leaf (Match.Expr.TableAttr (make_attr_iu attr1)) in
        let rhs = Match.Expr.Leaf (Match.Expr.TableAttr (make_attr_iu attr2)) in
        Match.Expr.Eq (lhs, rhs)
  in
  Match.Expr.And (List.map make_expr pred_lst)


let make_operator_tree db = function
  | Select (attr_lst, tbl_lst, pred_lst) ->
      let tbls = List.map (Binder.find_table db) tbl_lst in
      let tbl_scan =
        let tbl = List.hd tbls in
        TableScan { tbl; tuple_idx = 0; attr_idxs = [] }
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
      ( match pred_lst with
      | Some xs ->
          let pred_tree = make_match_tree db xs in
          (* TODO: create multiple Selection operators for every predicate.
           *         merge them later in the optimization step *)
          Projection (attrs, Selection (pred_tree, tbl_scan))
      | None ->
          Projection (attrs, tbl_scan) )


let run db ast =
  let tree = prepare [] @@ make_operator_tree db ast in
  let ctx = () in
  let rec exec acc =
    match next ctx tree with Some t -> exec (t :: acc) | None -> acc
  in
  exec []
