open Core
open Ctypes
module WT = Wired_tiger

(* TODO: refactor and potentially remove this *)
module Wired_tiger = Wired_tiger.Tiger

module Result = struct
  include Result

  (* maps wiredtiger error code to result with a message *)
  let of_code code ok err = if code = 0 then Result.Ok ok else Result.Error err
end

type record_id = Record.Id.t
type record_data = Record.Data.t
type record = Record.t

module Session = struct
  type t = WT.Session.t ptr
  type session_t = t

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

  module Cursor = struct
    type t = WT.Cursor.t ptr

    let make session tbl_name =
      let cursor_ptr_ptr = WT.Cursor.alloc_ptr () in
      let code =
        WT.Session.open_cursor session ("table:" ^ tbl_name) null ""
          cursor_ptr_ptr
      in
      Result.of_code code !@cursor_ptr_ptr `FailedCursorOpen

    let get_key cursor =
      let record_id_ptr = allocate uint64_t @@ Unsigned.UInt64.zero in
      let code = WT.Cursor.get_key cursor record_id_ptr in
      Result.of_code code !@record_id_ptr `FailedCursorGetKey

    let get_value cursor =
      let open Result in
      let record_data_ptr = WT.Item.alloc (Ctypes.make WT.Item.t) in
      let code = WT.Cursor.get_value cursor record_data_ptr in
      Result.of_code code !@record_data_ptr `FailedCursorGetValue
      >>| WT.Item.to_bytes

    let set_key = WT.Cursor.set_key

    let set_value cursor value =
      let item = WT.Item.alloc @@ WT.Item.of_bytes value in
      WT.Cursor.set_value cursor item

    let insert cursor =
      let code = WT.Cursor.insert cursor in
      Result.of_code code () `FailedCursorInsert

    let remove cursor =
      let code = WT.Cursor.remove cursor in
      Result.of_code code () `FailedCursorRemove

    let search cursor key =
      set_key cursor key;
      if Int.(WT.Cursor.search cursor = 0) then
        match get_value cursor with Ok x -> Some x | _ -> None
      else None

    (* let search_near cursor key = *)
    (*    let exact_ptr = allocate int 0 in *)
    (*    let code = WT.Cursor.search_near cursor_ptr exact_ptr in *)

    let next cursor =
      let code = WT.Cursor.next cursor in
      Result.of_code code () `FailedCursorNext

    let close cursor =
      let code = WT.Cursor.close cursor in
      Result.of_code code () `FailedCursorClose
  end
end

let make () =
  Wired_tiger.init_and_open_session
    ~path:"/Users/denis.grebennicov/Documents/lego_db/db"
    ~config:
      "create, direct_io=[data, log, checkpoint], log=(enabled,recover=on), \
       session_max=2000, cache_size=4096M"
