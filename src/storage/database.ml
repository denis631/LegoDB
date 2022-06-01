type t = {
  mutable tables : Table.t list;
  db_session_ref : Wired_tiger.session_ref;
}

let create () : t =
  let session_ref =
    Wired_tiger.init_and_open_session
      ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
      ~config:
        "create, direct_io=[data, log, checkpoint], log=(enabled=false), \
         session_max=2000, cache_size=4096M"
      ~isolation_config:Wired_tiger.IsolationLevelConfig.Snapshot
  in
  { tables = []; db_session_ref = session_ref }

let db_session_ref db = db.db_session_ref
let tbls t = t.tables

let create_tbl db tbl =
  (* TODO: write the tbl metadata into tbl catalog, which stores metadata about tables *)
  Wired_tiger.Table.create ~session_ref:(db_session_ref db)
    ~tbl_name:(Table.name tbl) ~config:"key_format=u,value_format=u";
  db.tables <- tbl :: db.tables
