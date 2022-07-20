open Core

type record_id = Record.Id.t
type record = Record.t

module Session = struct
  type t = Wired_tiger.session_ref

  module Crud = struct
    module Table = struct
      let exists session_ref tbl_name =
        Wired_tiger.Table.exists ~session_ref ~tbl_name

      let create session_ref tbl_name =
        Wired_tiger.Table.create ~session_ref ~tbl_name
          ~config:"key_format:r,value_format:u"

      let drop session_ref tbl_name =
        Wired_tiger.Table.drop ~session_ref ~tbl_name ~config:""
    end

    module Record = struct
      let insert session_ref tbl_name record =
        Wired_tiger.Record.insert_one ~session_ref ~tbl_name ~record

      let bulk_insert session_ref tbl_name records =
        Wired_tiger.Record.bulk_insert ~session_ref ~tbl_name ~records

      let read_all session_ref tbl_name =
        let scanner = Wired_tiger.Record.scan ~session_ref ~tbl_name in
        let generator f =
          match f () with Some record -> Some (record, f) | None -> None
        in
        Sequence.unfold ~init:scanner ~f:generator

      let delete session_ref tbl_name key =
        Wired_tiger.Record.delete_one ~session_ref ~tbl_name ~key
    end
  end
end

let make () =
  Wired_tiger.init_and_open_session
    ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
    ~config:
      "create, direct_io=[data, log, checkpoint], log=(enabled,recover=on), \
       session_max=2000, cache_size=4096M"
