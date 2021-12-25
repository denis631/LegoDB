open Storage
open BatteriesExceptionless
open Common

type tbl_scan =
  { iter : Table.Iter.t
  ; ius : Table.Iu.t list
  }

type op += TableScan of tbl_scan

let make ~iter ~ius = TableScan { iter; ius }

let has_iu _ iu tbl_scan = List.exists (Table.Iu.eq iu) tbl_scan.ius

let prepare _ tbl_scan = tbl_scan

let next _ tbl_scan =
  Option.map (fun t -> (t, tbl_scan.ius)) @@ Table.Iter.next tbl_scan.iter
