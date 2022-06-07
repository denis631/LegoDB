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
    let open_cursor () = open_cursor ~session_ref ~tbl_name ~config:"raw" in
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
  type t = Bytearray.t

  let lookup_one ~session_ref ~tbl_name ~key =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:"raw"
    in
    let search_for_key cursor_ptr =
      Cursor.set_key cursor_ptr (Item.alloc (Item.of_bytes key));
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
        Result.of_code (Cursor.reset cursor_ptr) (Item.to_bytes !@item_ptr)
          "Couldn't reset the cursor"
      in
      get_value () >>= reset_cursor
    in
    get_cursor_ptr () >>= search_for_key >>= get_value |> Option.of_result

  let insert_one ~session_ref ~tbl_name ~key ~record =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:"raw"
    in
    let insert cursor_ptr =
      let set_data () =
        let item_key = Item.alloc @@ Item.of_bytes key in
        let item_value = Item.alloc @@ Item.of_bytes record in
        Cursor.set_key cursor_ptr item_key;
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
      Table.open_cursor ~session_ref ~tbl_name ~config:"raw"
    in
    let delete cursor_ptr =
      let item_key = Item.alloc @@ Item.of_bytes key in
      Cursor.set_key cursor_ptr item_key;
      Result.of_code (Cursor.remove cursor_ptr) cursor_ptr
        "Couldn't delete data with the cursor"
    in
    let close_cursor cursor_ptr =
      Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
    in
    get_cursor_ptr () >>= delete >>= close_cursor |> ok_or_failwith

  let bulk_insert ~session_ref ~tbl_name ~keys_and_records =
    let open Result in
    assert (not @@ is_null session_ref);
    let get_cursor_ptr () =
      Table.open_cursor ~session_ref ~tbl_name ~config:"bulk,raw"
    in
    let perform_writes cursor_ptr =
      (* Records can only be bulk inserted in the sorted order *)
      let cmp (lhs, _) (rhs, _) =
        let lhs_l, rhs_l = (Bytearray.length lhs, Bytearray.length rhs) in
        let rec f = function
          | idx when lhs_l = idx && rhs_l = idx -> 0
          | idx when lhs_l > idx && rhs_l = idx -> 1
          | idx when lhs_l < idx && rhs_l = idx -> -1
          | idx -> (
              let x, y =
                (Bigarray.Array1.get lhs idx, Bigarray.Array1.get rhs idx)
              in
              match (Char.to_int x, Char.to_int y) with
              | a, b when a < b -> -1
              | a, b when a > b -> 1
              | _ -> f (idx + 1))
        in
        f 0
      in
      let perform_write key record cursor_ptr =
        let item_key = Item.of_bytes key in
        let item_value = Item.of_bytes record in
        Cursor.set_key cursor_ptr @@ addr item_key;
        Cursor.set_value cursor_ptr @@ addr item_value;
        Result.of_code (Cursor.insert cursor_ptr) cursor_ptr
          "Couldn't insert data with the cursor"
      in
      keys_and_records |> List.sort ~compare:cmp
      |> List.fold_left ~init:(Result.Ok cursor_ptr)
           ~f:(fun acc (key, record) -> acc >>= perform_write key record)
    in
    let close_cursor cursor_ptr =
      Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
    in
    get_cursor_ptr () >>= perform_writes >>= close_cursor |> ok_or_failwith

  let scan ~session_ref ~tbl_name =
    let open Result in
    assert (not @@ is_null session_ref);
    let cursor_ptr =
      Table.open_cursor ~session_ref ~tbl_name ~config:"raw" |> ok_or_failwith
    in
    (* Set the minKey to 0, such that we always start from the first element in the table *)
    let minKey =
      let byte0 = CArray.make char ~initial:(Char.unsafe_of_int 0) 1 in
      Item.of_bytes (bigarray_of_array array1 Bigarray.Char byte0)
    in
    Cursor.set_key cursor_ptr (addr minKey);
    let cur_code =
      let code =
        let exact_ptr = allocate int 0 in
        let code = Cursor.search_near cursor_ptr exact_ptr in
        if !@exact_ptr < 0 then Cursor.next cursor_ptr else code
      in
      ref code
    in
    (* Allocate only one item and reuse it throughout the iterations *)
    let item_ptr = Item.alloc (Ctypes.make Item.t) in
    let next () =
      if not (0 = !cur_code) then (
        assert_cmd @@ Cursor.close cursor_ptr;
        None)
      else
        let value =
          assert_cmd @@ Cursor.get_value cursor_ptr item_ptr;
          Item.to_bytes !@item_ptr
        in
        cur_code := Cursor.next cursor_ptr;
        Some value
    in
    next
end
