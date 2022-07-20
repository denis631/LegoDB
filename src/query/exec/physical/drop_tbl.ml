open Common
open Core
open Utils

type drop_tbl = { metas : TableMeta.t list }
type op += DropTbl of drop_tbl

let make ~tbl_metas = DropTbl { metas = tbl_metas }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ drop_tbl =
  List.iter ~f:(Catalog.drop_tbl Catalog.instance) drop_tbl.metas;
  None
