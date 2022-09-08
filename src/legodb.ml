open Core
open Storage
open Utils
module TableMeta = Table_meta

type t = { session : Storage.Database.Session.t; catalog : Catalog.t }

let make () =
  let path = "/Users/denis.grebennicov/Documents/lego_db/db" in
  let session = Database.Session.make ~path () in
  let catalog = Catalog.make () in
  let catalog_tables =
    let ctx : Executor.Common.ctx = { session; catalog } in
    let create_tbl () =
      let create_tbl_op = Executor.Create_tbl.make ~tbl_meta:Catalog.meta in
      Executor.Operators.open_op ctx create_tbl_op;
      let _ = Executor.Operators.next ctx create_tbl_op in
      Executor.Operators.close_op ctx create_tbl_op
    in
    let read_from_catalog () =
      let tbl_scan = Executor.Table_scan.make ~meta:Catalog.meta in
      Executor.Operators.open_op ctx tbl_scan;
      let seq_of_tree tree =
        let next ctx =
          match Executor.Operators.next ctx tree with
          | Some (id, data) ->
              let meta = TableMeta.Marshaller.unmarshal data in
              meta.tid <- id;
              Some (meta, ctx)
          | None ->
              Executor.Operators.close_op ctx tree;
              None
        in
        Sequence.unfold ~init:ctx ~f:next
      in
      seq_of_tree tbl_scan |> Sequence.to_list
    in
    create_tbl ();
    read_from_catalog ()
  in
  Catalog.set_catalog_tables catalog catalog_tables;
  { session; catalog }

let instance = make ()
