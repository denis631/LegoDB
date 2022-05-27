type t = { mutable tables : Table.t list; db_ref : Wired_tiger.t }

let create () : t =
  let wired_tiger =
    let ref =
      Wired_tiger.init ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
        ~config:
          "create, direct_io=[data, log, checkpoint], log=(enabled=false), \
           session_max=2000, cache_size=4096M"
    in
    Wired_tiger.open_session ~db:ref
      ~config:Wired_tiger.IsolationLevelConfig.Snapshot
  in
  { tables = []; db_ref = wired_tiger }

let db_ref db = db.db_ref
let tbls t = t.tables

let create_tbl db tbl =
  (* TODO: write the tbl metadata into tbl catalog, which stores metadata about tables *)
  Wired_tiger.create_tbl ~db:(db_ref db) ~tbl_name:(Table.name tbl)
    ~config:"key_format=u,value_format=u";
  db.tables <- tbl :: db.tables
