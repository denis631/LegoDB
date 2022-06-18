open Storage
open BatteriesExceptionless
open Common

type tbl_scan = {
  meta : Table.T.Meta.t;
  ius : Table.T.Iu.t list;
  mutable seq : Storage.Tuple.t Core.Sequence.t option;
}

type op += TableScan of tbl_scan

let make ~meta ~ius = TableScan { meta; ius; seq = None }
let has_iu _ iu tbl_scan = List.exists (Table.T.Iu.eq iu) tbl_scan.ius

let open_op tbl_scan =
  tbl_scan.seq <-
    Some
      (Table.T.Crud.Record.read_all
         (Database.db_session_ref Database.instance)
         tbl_scan.meta)

let next _ tbl_scan =
  match Core.Sequence.next @@ Option.get tbl_scan.seq with
  | Some (el, seq) ->
      tbl_scan.seq <- Some seq;
      Some (el, tbl_scan.ius)
  | None -> None
