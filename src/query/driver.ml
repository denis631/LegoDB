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
      let find_tbl = Catalog.find_table @@ Database.catalog db in
      let tbl_meta_seq = Sequence.of_list tbl_lst |> Sequence.map ~f:find_tbl in
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
          tbl_lst |> List.map ~f:find_tbl
          |> List.map ~f:(fun (meta : Table.Meta.t) -> meta.schema)
          |> List.concat
        else
          Sequence.of_list attr_lst
          |> Sequence.filter_map ~f:(function
               | AttrName s -> Some s
               | Star -> None)
          |> Sequence.map
               ~f:(Binder.find_column_attr (Sequence.to_list tbl_meta_seq))
          |> Sequence.to_list
      in
      let select_op =
        let mk_select child pred = Logical.Operators.Selection (pred, child) in
        let match_expr =
          let bool_preds =
            Option.value ~default:[] pred_lst
            |> List.map ~f:(make_expr (Sequence.to_list tbl_meta_seq))
          in
          Match.Expr.And bool_preds
        in
        mk_select tbl_scan match_expr
      in
      (* TODO: mark projection as star, in case it's all attrs projection, to
               further eliminate this stage if possible *)
      Logical.Operators.Projection (attrs, select_op)
  | Copy (tbl_name, path) -> Logical.Operators.Copy (tbl_name, path)
  | CreateTbl (tbl_name, tbl_elt_lst) ->
      let cols_and_indexes =
        List.fold_right
          ~f:(fun tbl_elt acc ->
            match tbl_elt with
            | ColDef (column, ty) ->
                (Schema.Iu.make ~table:tbl_name ~column ~ty :: fst acc, snd acc)
            | ConstraintDef (PrimaryKey, cols) ->
                (fst acc, Index.PrimaryIdx cols :: snd acc))
          ~init:([], []) tbl_elt_lst
      in
      let tbl_meta =
        Table.Meta.make ~name:tbl_name ~schema:(fst cols_and_indexes)
          ~indexes:(snd cols_and_indexes) ()
      in
      Logical.Operators.CreateTbl tbl_meta
  | DropTbl tbls ->
      Logical.Operators.DropTbl
        (List.map ~f:(Catalog.find_table (Database.catalog db)) tbls)

let run db ast f =
  let seq_of_tree tree =
    let schema = Physical.Operators.output_schema tree in
    let next ctx =
      match Physical.Operators.next ctx tree with
      (* TODO: think how to improve it *)
      | Some t -> Some (Storage.Tuple.show t schema, ctx)
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
  |> seq_of_tree |> Sequence.iter ~f

let benchmark f =
  let t = Stdlib.Sys.time () in
  let log _ = Printf.printf "Execution time: %fs\n" (Stdlib.Sys.time () -. t) in
  f () |> tap log
