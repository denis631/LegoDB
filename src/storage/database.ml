type t = {
  (* TODO: add catalog intead of list of tables *)
  mutable catalog : Catalog.t;
  db_session_ref : Wired_tiger.session_ref;
}

let create () : t =
  let session_ref =
    Wired_tiger.init_and_open_session
      ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
      ~config:
        "create, direct_io=[data, log, checkpoint], log=(enabled,recover=on), \
         session_max=2000, cache_size=4096M"
  in
  { catalog = Catalog.create session_ref; db_session_ref = session_ref }

let db_session_ref db = db.db_session_ref
let catalog db = db.catalog
let instance = create ()
let create_tbl db tbl = Catalog.create_tbl db.catalog db.db_session_ref tbl
let drop_tbl db tbl = Catalog.drop_tbl db.catalog db.db_session_ref tbl
