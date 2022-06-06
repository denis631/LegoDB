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
let create_tbl db tbl = Catalog.create_tbl db.catalog db.db_session_ref tbl

let load_data db tbl_meta path =
  let get_file_data path =
    let read chan =
      try Some (input_line chan, chan)
      with End_of_file ->
        close_in chan;
        None
    in
    Core.Sequence.unfold ~init:(open_in path) ~f:read
  in
  get_file_data path
  |> Core.Sequence.map ~f:(Tuple.parse @@ Table.T.Meta.schema tbl_meta)
  |> Table.T.Crud.bulk_insert db.db_session_ref tbl_meta
