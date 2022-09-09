open Binder.Ast
open Core
open Utils
open Utils.Fp
open Storage
module TableMeta = Table_meta

let make_operator_tree catalog = function
  | Select (attr_lst, FromClause tbl_lst, where_clause, _, limit_clause) -> (
      let find_tbl = Catalog.find_tbl catalog in
      let tbl_meta_seq = Sequence.of_list tbl_lst |> Sequence.map ~f:find_tbl in
      let tbl_scan =
        let mk_tbl_scan tbl = Planner.Node.TableScan tbl in
        let mk_nlj lhs rhs = Planner.Node.Join (lhs, rhs, ([], []), NLJ) in
        tbl_meta_seq
        |> Sequence.map ~f:mk_tbl_scan
        |> Sequence.reduce ~f:mk_nlj |> Stdlib.Option.get
      in
      let attrs =
        if List.exists ~f:(phys_equal Star) attr_lst then Planner.Node.All
        else
          let schema =
            Sequence.of_list attr_lst
            |> Sequence.filter_map ~f:(function
                 | Attr iu -> Some iu
                 | Star -> None)
            |> Sequence.to_list
          in
          Planner.Node.Attributes schema
      in
      let select_op =
        match where_clause with
        | Some (WhereClause match_expr) ->
            Planner.Node.Selection (tbl_scan, match_expr)
        | None -> tbl_scan
      in
      let projection = Planner.Node.Projection (select_op, attrs) in
      match limit_clause with
      | None -> projection
      | Some (LimitClause limit) -> Planner.Node.Limit (projection, limit))
  | Copy (tbl_name, path) -> Planner.Node.Copy (tbl_name, path)
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
        TableMeta.make ~name:tbl_name ~schema:(fst cols_and_indexes)
          ~indexes:(snd cols_and_indexes) ()
      in
      Planner.Node.CreateTbl tbl_meta
  | DropTbl tbls ->
      Planner.Node.DropTbl (List.map ~f:(Catalog.find_tbl catalog) tbls)

let run (legodb : Legodb.t) ast f =
  let catalog = legodb.catalog in
  let ctx : Executor.Common.ctx = { session = legodb.session; catalog } in
  let seq_of_tree tree =
    let schema = Executor.Operators.output_schema tree in
    let next ctx =
      match Executor.Operators.next ctx tree with
      | Some t -> Some (Record.Data.show (snd t) schema, ctx)
      | None ->
          Executor.Operators.close_op ctx tree;
          None
    in
    let init () =
      Executor.Operators.open_op ctx tree;
      ctx
    in
    Sequence.unfold ~init:(init ()) ~f:next
  in
  Binder.bind catalog ast |> make_operator_tree catalog
  |> tap (Planner.Node.show %> print_endline)
  |> Optimizer.optimize catalog |> seq_of_tree |> Sequence.iter ~f

let benchmark f =
  let t = Stdlib.Sys.time () in
  let log _ = Printf.printf "Execution time: %fs\n" (Stdlib.Sys.time () -. t) in
  f () |> tap log
