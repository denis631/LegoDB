open Ctypes
open Core

module Result = struct
  include Result

  (* maps wiredtiger error code to result with a message *)
  let of_code code res msg =
    if code = 0 then Result.Ok res else Result.Error msg
end

module Option = struct
  include Option

  let of_result = function Ok x -> Some x | Error _ -> None
end

let assert_cmd cmd_code = assert (0 = cmd_code)

type session_ref = Session.t ptr

(* Establishes a connection and creates a new session_ref *)
let init_and_open_session ~path ~config =
  let open Result in
  let open_connection () =
    let conn_ptr_ptr = Connection.alloc_ptr () in
    Result.of_code
      (Bindings.WiredTiger.wiredtiger_open path null config conn_ptr_ptr)
      conn_ptr_ptr "Couldn't create the database"
  in
  let open_session conn_ptr_ptr =
    assert (not (is_null !@conn_ptr_ptr));
    let conn_ptr = !@conn_ptr_ptr in
    let session_ptr_ptr = Session.alloc_ptr () in
    let code =
      Connection.open_session conn_ptr null "isolation=snapshot" session_ptr_ptr
    in
    Result.of_code code !@session_ptr_ptr "Couldn't create the session"
  in
  open_connection () >>= open_session |> ok_or_failwith

module Txn = struct
  let begin_txn session_ref =
    Result.of_code
      (Session.begin_txn session_ref "")
      () "Couldn't begin the transaction"
    |> Result.ok_or_failwith

  let commit_txn session_ref =
    Result.of_code
      (Session.commit_txn session_ref "")
      () "Couldn't commit the transaction"
    |> Result.ok_or_failwith

  let rollback_txn session_ref =
    Result.of_code
      (Session.rollback_txn session_ref "")
      () "Couldn't rollback the transaction"
    |> Result.ok_or_failwith
end

module Table = struct
  let open_cursor ~session_ref ~tbl_name ~config =
    assert (not @@ is_null session_ref);
    let cursor_ptr_ptr = Cursor.alloc_ptr () in
    let code =
      Session.open_cursor session_ref ("table:" ^ tbl_name) null config
        cursor_ptr_ptr
    in
    Result.of_code code !@cursor_ptr_ptr "Couldn't open a cursor"

  let exists ~session_ref ~tbl_name =
    let open Result in
    let open_cursor () = open_cursor ~session_ref ~tbl_name ~config:"" in
    let close_cursor cursor_ptr =
      Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
    in
    open_cursor () >>= close_cursor |> Result.is_ok

  let create ~session_ref ~tbl_name ~config =
    assert (not @@ is_null session_ref);
    if not @@ (0 = Session.create_tbl session_ref ("table:" ^ tbl_name) config)
    then failwith "Couldn't create the session"

  let drop ~session_ref ~tbl_name ~config =
    assert (not @@ is_null session_ref);
    if not @@ (0 = Session.drop_tbl session_ref ("table:" ^ tbl_name) config)
    then failwith "Couldn't drop the table"
end

module Record = struct
  type record_id = Unsigned.UInt64.t
  type record_data = Bytearray.t
  type t = record_id * record_data

  let insert_one ~session_ref ~tbl_name ~record =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:"append"
    in
    let insert cursor_ptr =
      let set_data () =
        let item_value = Item.alloc @@ Item.of_bytes (snd record) in
        Cursor.set_value cursor_ptr item_value
      in
      set_data ();
      Result.of_code (Cursor.insert cursor_ptr) cursor_ptr
        "Couldn't insert data with the cursor"
    in
    let close_cursor cursor_ptr =
      Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
    in
    get_cursor_ptr () >>= insert >>= close_cursor |> ok_or_failwith

  let delete_one ~session_ref ~tbl_name ~key =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:""
    in
    let delete cursor_ptr =
      Cursor.set_key cursor_ptr key;
      Result.of_code (Cursor.remove cursor_ptr) cursor_ptr
        "Couldn't delete data with the cursor"
    in
    let close_cursor cursor_ptr =
      Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
    in
    get_cursor_ptr () >>= delete >>= close_cursor |> ok_or_failwith

  let lookup_one ~session_ref ~tbl_name ~key =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:""
    in
    let search_for_key cursor_ptr =
      Cursor.set_key cursor_ptr key;
      Result.of_code (Cursor.search cursor_ptr) cursor_ptr "Nothing found"
    in
    let get_value cursor_ptr =
      let item_ptr = Item.alloc (Ctypes.make Item.t) in
      let get_value () =
        Result.of_code
          (Cursor.get_value cursor_ptr item_ptr)
          () "Couldn't get the value"
      in
      let reset_cursor () =
        Result.of_code (Cursor.reset cursor_ptr)
          (key, Item.to_bytes !@item_ptr)
          "Couldn't reset the cursor"
      in
      get_value () >>= reset_cursor
    in
    get_cursor_ptr () >>= search_for_key >>= get_value |> Option.of_result

  let scan ~session_ref ~tbl_name =
    let open Result in
    assert (not @@ is_null session_ref);
    let cursor_ptr =
      Table.open_cursor ~session_ref ~tbl_name ~config:"" |> ok_or_failwith
    in
    (* Set the minKey to 1, such that we always start from the first element in the table *)
    let minKey = Unsigned.UInt64.of_int 1 in
    Cursor.set_key cursor_ptr minKey;
    let cur_code =
      let code =
        let exact_ptr = allocate int 0 in
        let code = Cursor.search_near cursor_ptr exact_ptr in
        if !@exact_ptr < 0 then Cursor.next cursor_ptr else code
      in
      ref code
    in
    (* Allocate only one item and reuse it throughout the iterations *)
    let record_id_ptr = allocate uint64_t @@ Unsigned.UInt64.zero in
    let record_data_ptr = Item.alloc (Ctypes.make Item.t) in
    let next () =
      if not (0 = !cur_code) then (
        assert_cmd @@ Cursor.close cursor_ptr;
        None)
      else
        let key =
          assert_cmd @@ Cursor.get_key cursor_ptr record_id_ptr;
          !@record_id_ptr
        in
        let value =
          assert_cmd @@ Cursor.get_value cursor_ptr record_data_ptr;
          Item.to_bytes !@record_data_ptr
        in
        cur_code := Cursor.next cursor_ptr;
        Some (key, value)
    in
    next
end
