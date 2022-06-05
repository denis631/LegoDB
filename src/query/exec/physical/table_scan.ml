open Storage
open BatteriesExceptionless
open Common

type tbl_scan = {
  mutable seq : Storage.Tuple.t Core.Sequence.t;
  ius : Table.T.Iu.t list;
}

type op += TableScan of tbl_scan

let make ~seq ~ius = TableScan { seq; ius }
let has_iu _ iu tbl_scan = List.exists (Table.T.Iu.eq iu) tbl_scan.ius
let prepare _ tbl_scan = tbl_scan

let next _ tbl_scan =
  match Core.Sequence.next tbl_scan.seq with
  | Some (el, seq) ->
      tbl_scan.seq <- seq;
      Some (el, tbl_scan.ius)
  | None -> None
