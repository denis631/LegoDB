type t = { mutable tables : Table.t list; db_session : Wired_tiger.session }

let create () : t =
  let session =
    Wired_tiger.init_and_open_session
      ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
      ~config:
        "create, direct_io=[data, log, checkpoint], log=(enabled=false), \
         session_max=2000, cache_size=4096M"
      ~isolation_config:Wired_tiger.IsolationLevelConfig.Snapshot
  in
  { tables = []; db_session = session }

let db_session db = db.db_session
let tbls t = t.tables

let create_tbl db tbl =
  (* TODO: write the tbl metadata into tbl catalog, which stores metadata about tables *)
  Wired_tiger.create_tbl ~session:(db_session db) ~tbl_name:(Table.name tbl)
    ~config:"key_format=u,value_format=u";
  db.tables <- tbl :: db.tables
