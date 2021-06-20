open Frontend.Ast
open Storage
open Expr

let make_expr db pred =
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


let make_match_tree db pred_lst =
  Match.Expr.And (List.map (make_expr db) pred_lst)


let make_operator_tree db = function
  | Select (attr_lst, tbl_lst, pred_lst) ->
      let tbls = List.map (Binder.find_table db) tbl_lst in
      let tbl_scan =
        let tbl_scans =
          List.map (fun tbl -> Logical.Operators.TableScan tbl) tbls
        in
        BatList.reduce
          (fun acc op -> Logical.Operators.CrossProduct (acc, op))
          tbl_scans
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
      let select_ops =
        pred_lst
        |> Option.value ~default:[]
        |> List.map (make_expr db)
        |> List.fold_left
             (fun acc pred -> Logical.Operators.Selection (pred, acc))
             tbl_scan
      in
      Logical.Operators.Projection (attrs, select_ops)


let run db ast f =
  let tree =
    ast
    |> make_operator_tree db
    |> Optimizer.optimize
    |> Physical.Operators.prepare []
  in
  let ctx = () in
  let rec iter () =
    match Physical.Operators.next ctx tree with
    | Some t ->
        f t ;
        iter ()
    | None ->
        ()
  in
  iter ()


let benchmark f =
  let t = Sys.time () in
  let result = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t) ;
  result
