open Common
open Core
open Storage

type tbl_scan = { meta : Table.Meta.t; seq : Row.t Core.Sequence.t }
type op += TableScan of tbl_scan

let make ~meta =
  let seq =
    Table.Crud.Record.read_all (Database.db_session_ref Database.instance) meta
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
