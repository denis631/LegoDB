open Core
open Storage
open Utils

module TableMeta = Table_meta

type t = {
   session : Storage.Database.Session.t;
   catalog : Catalog.t;
}

let make () = 
   let path = "/Users/denis.grebennicov/Documents/lego_db/db" in
   let session = Database.Session.make ~path () in
   let catalog = Catalog.make () in
   let catalog_tables =
      let tbl_scan = Physical.Table_scan.make ~meta:Catalog.meta in
      let ctx: Physical.Common.ctx = { session; catalog } in
      Physical.Operators.open_op ctx tbl_scan;
      let seq_of_tree tree =
         let next ctx =
            match Physical.Operators.next ctx tree with
            | Some (id, data) -> 
                let meta = TableMeta.Marshaller.unmarshal data in
                meta.tid <- id;
                Some (meta, ctx)
            | None ->
                Physical.Operators.close_op ctx tree;
                None
       in
       Sequence.unfold ~init:ctx ~f:next
      in
      seq_of_tree tbl_scan |> Sequence.to_list
   in
   Catalog.set_catalog_tables catalog catalog_tables;
   { session; catalog }

let instance = make ()

