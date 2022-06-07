open Frontend.Ast
open Storage
open Expr
open Utils

let make_expr db pred =
  let make_attr_iu = function
    | AttrName x -> Binder.find_column_attr db x
    | _ -> failwith "TODO: support this use-case"
  in
  let make_const = function
    | Int i ->
        Match.Expr.Leaf (Match.Expr.Const (Value.Integer (Int64.of_int i)))
    | Str s -> Match.Expr.Leaf (Match.Expr.Const (Value.StringLiteral s))
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
      let tbls = List.map (Catalog.find_table @@ Database.catalog db) tbl_lst in
      let tbl_scan =
        let tbl_scans =
          List.map (fun tbl -> Logical.Operators.TableScan tbl) tbls
        in
        BatList.reduce
          (fun acc op -> Logical.Operators.CrossProduct (acc, op))
          tbl_scans
      in
      let attrs =
        if List.exists (( = ) Star) attr_lst then
          failwith "TODO: implement projection of all attributes"
        else
          attr_lst
          |> List.filter_map (fun attr ->
                 match attr with AttrName s -> Some s | Star -> None)
          |> List.map (Binder.find_column_attr db)
      in
      let select_ops =
        pred_lst |> Option.value ~default:[]
        |> List.map (make_expr db)
        |> List.fold_left
             (fun acc pred -> Logical.Operators.Selection (pred, acc))
             tbl_scan
      in
      Logical.Operators.Projection (attrs, select_ops)
  | _ -> failwith "Invalid code path"

let run_ddl db = function
  | CreateTbl (tbl_name, tbl_elt_lst) ->
      let schema_of_col_defs tbl_elt_lst =
        let cols =
          let get_cols = function
            | ColDef (col, ty) -> Some (col, ty)
            | _ -> None
          in
          List.filter_map get_cols tbl_elt_lst
        in
        let primary_key_cols =
          let get_cols = function
            | ConstraintDef (PrimaryKey, cols) -> Some cols
            | _ -> None
          in
          List.filter_map get_cols tbl_elt_lst |> List.flatten
        in
        (cols, primary_key_cols)
      in
      let schema = schema_of_col_defs tbl_elt_lst in
      Database.create_tbl db @@ Table.T.Meta.make tbl_name schema
  | DropTbl tbls ->
      tbls
      |> List.map (Catalog.find_table (Database.catalog db))
      |> List.iter (Database.drop_tbl db)

let run db ast f =
  match ast with
  | DDL cmd -> run_ddl db cmd
  | DML (Copy (tbl_name, path)) ->
      let tbl_meta = Catalog.find_table (Database.catalog db) tbl_name in
      Database.load_data db tbl_meta path
  | DML cmd ->
      let tree =
        make_operator_tree db cmd
        |> tap @@ (Logical.Operators.show %> print_endline)
        |> Optimizer.optimize db
        |> Physical.Operators.prepare []
      in
      let ctx = () in
      let rec iter () =
        match Physical.Operators.next ctx tree with
        | Some (t, _) ->
            f t;
            iter ()
        | None -> ()
      in
      iter ()

let benchmark f =
  let t = Sys.time () in
  let result = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  result
