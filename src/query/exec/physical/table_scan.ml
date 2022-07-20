open Common
open Core
open Storage
open Utils

type tbl_scan = { meta : TableMeta.t; seq : Record.t Core.Sequence.t }
type op += TableScan of tbl_scan

let make ~(meta : TableMeta.t) =
  (* TODO: not sure the session should be tied by catalog *)
  let seq =
    Database.Session.Crud.Record.read_all
      (Catalog.session Catalog.instance)
      meta.name
  in
  TableScan { meta; seq }

let has_iu _ iu tbl_scan =
  List.exists ~f:(Schema.Iu.equal iu) tbl_scan.meta.schema

(* Nothing has to be done as sequence is already lazy and is not opened in make *)
let open_op _ = ()

(* Nothing has to be done, as cursor cleanup is done in the WiredTiger Sequence itself *)
let close_op _ = ()

let next _ tbl_scan =
  let open Option in
  Core.Sequence.next tbl_scan.seq >>| fst
