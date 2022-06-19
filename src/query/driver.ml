open Frontend.Ast
open Storage
open Expr
open Utils
open Core

let make_expr tbl_meta_lst pred =
  let make_attr_iu = function
    | AttrName x -> Binder.find_column_attr tbl_meta_lst x
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
  Match.Expr.And (List.map ~f:(make_expr db) pred_lst)

let make_operator_tree db = function
  | Select (attr_lst, tbl_lst, pred_lst) ->
      let tbl_meta_seq =
        let find_tbl = Catalog.find_table @@ Database.catalog db in
        Sequence.of_list tbl_lst |> Sequence.map ~f:find_tbl
      in
      let tbl_scan =
        let mk_tbl_scan tbl = Logical.Operators.TableScan tbl in
        let mk_cross_product lhs rhs =
          Logical.Operators.CrossProduct (lhs, rhs)
        in
        tbl_meta_seq
        |> Sequence.map ~f:mk_tbl_scan
        |> Sequence.reduce ~f:mk_cross_product
        |> Stdlib.Option.get
      in
      let attrs =
        if List.exists ~f:(phys_equal Star) attr_lst then
          failwith "TODO: implement projection of all attributes"
        else
          Sequence.of_list attr_lst
          |> Sequence.filter_map ~f:(function
               | AttrName s -> Some s
               | Star -> None)
          |> Sequence.map
               ~f:(Binder.find_column_attr (Sequence.to_list tbl_meta_seq))
          |> Sequence.to_list
      in
      let select_ops =
        let mk_select child pred = Logical.Operators.Selection (pred, child) in
        Option.value ~default:[] pred_lst
        |> Sequence.of_list
        |> Sequence.map ~f:(make_expr (Sequence.to_list tbl_meta_seq))
        |> Sequence.fold ~f:mk_select ~init:tbl_scan
      in
      Logical.Operators.Projection (attrs, select_ops)
  | Copy (tbl_name, path) -> Logical.Operators.Copy (tbl_name, path)
  | CreateTbl (tbl_name, tbl_elt_lst) ->
      let cols_and_indexes =
        List.fold_right
          ~f:(fun tbl_elt acc ->
            match tbl_elt with
            | ColDef (col, ty) -> ((col, ty) :: fst acc, snd acc)
            | ConstraintDef (PrimaryKey, cols) ->
                (fst acc, Index.PrimaryIdx cols :: snd acc))
          ~init:([], []) tbl_elt_lst
      in
      let tbl_meta =
        Table.T.Meta.make tbl_name (fst cols_and_indexes) (snd cols_and_indexes)
      in
      Logical.Operators.CreateTbl tbl_meta
  | DropTbl tbls ->
      Logical.Operators.DropTbl
        (List.map ~f:(Catalog.find_table (Database.catalog db)) tbls)

let run db ast f =
  let seq_of_tree tree =
    let next ctx =
      match Physical.Operators.next ctx tree with
      | Some x -> Some (x, ctx)
      | None ->
          Physical.Operators.close_op tree;
          None
    in
    Sequence.unfold ~init:() ~f:next
  in
  make_operator_tree db ast
  |> tap (Logical.Operators.show %> print_endline)
  |> Optimizer.optimize db
  |> tap Physical.Operators.open_op
  |> seq_of_tree |> Sequence.map ~f:fst |> Sequence.iter ~f

let benchmark f =
  let t = Stdlib.Sys.time () in
  let log _ = Printf.printf "Execution time: %fs\n" (Stdlib.Sys.time () -. t) in
  f () |> tap log
