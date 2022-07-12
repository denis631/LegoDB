open Common
open Core
open Storage

type drop_tbl = { metas : Table.Meta.t list }
type op += DropTbl of drop_tbl

let make ~tbl_metas = DropTbl { metas = tbl_metas }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ drop_tbl =
  List.iter ~f:(Database.drop_tbl Database.instance) drop_tbl.metas;
  None
