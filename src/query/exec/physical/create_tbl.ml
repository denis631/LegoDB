open Common
open Utils

type create_tbl = { meta : TableMeta.t }
type op += CreateTbl of create_tbl

let make ~tbl_meta = CreateTbl { meta = tbl_meta }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ create_tbl =
  Catalog.create_tbl Catalog.instance create_tbl.meta;
  None
