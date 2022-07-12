open Common
open Storage

type create_tbl = { meta : Table.Meta.t }
type op += CreateTbl of create_tbl

let make ~tbl_meta = CreateTbl { meta = tbl_meta }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ create_tbl =
  Database.create_tbl Database.instance create_tbl.meta;
  None
