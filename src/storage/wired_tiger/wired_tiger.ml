open Ctypes
open BatteriesExceptionless

module Result = struct
  include Result

  (* maps wiredtiger error code to result with a message *)
  let of_code code res msg =
    if code = 0 then Result.Ok res else Result.Error msg

  let get_ok_or_fail = function Ok x -> x | Error msg -> failwith msg
  let get_ok_or_none = function Ok x -> Some x | Error _ -> None
end

open Result.Infix

type t = Session.t ptr
type bin_repr_t = Bytearray.t

module IsolationLevelConfig = struct
  type t = Snapshot | ReadCommitted

  let show = function
    | Snapshot -> "isolation=snapshot"
    | ReadCommitted -> "isolation=read_committed"
end

(* Establishes a connection and creates a new session *)
let init_and_open_session ~path ~config ~isolation_config =
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
      Connection.open_session conn_ptr null
        (IsolationLevelConfig.show isolation_config)
        session_ptr_ptr
    in
    Result.of_code code !@session_ptr_ptr "Couldn't create the session"
  in
  open_connection () >>= open_session |> Result.get_ok_or_fail

let create_tbl ~session ~tbl_name ~config =
  assert (session != from_voidp Session.t null);
  if Session.create_tbl session ("table:" ^ tbl_name) config != 0 then
    failwith "Couldn't create the session"

let open_tbl_cursor ~session_ptr ~tbl_name ~config =
  assert (session_ptr != from_voidp Session.t null);
  let cursor_ptr_ptr = Cursor.alloc_ptr () in
  let code =
    Session.open_cursor session_ptr ("table:" ^ tbl_name) null config
      cursor_ptr_ptr
  in
  Result.of_code code !@cursor_ptr_ptr "Couldn't open a cursor"

let lookup_one ~session ~tbl_name ~key =
  assert (session != from_voidp Session.t null);
  let get_cursor_ptr () =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
  in
  let search_for_key cursor_ptr =
    Cursor.set_key cursor_ptr (Item.alloc (Item.of_bytes key));
    Result.of_code (Cursor.search cursor_ptr) cursor_ptr "Nothing found"
  in
  let get_value cursor_ptr =
    let item_ptr = Item.alloc (make Item.t) in
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
  get_cursor_ptr () >>= search_for_key >>= get_value |> Result.get_ok_or_none

let insert_record ~session ~tbl_name ~key ~record =
  assert (session != from_voidp Session.t null);
  let get_cursor_ptr () =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
  in
  let insert cursor_ptr =
    let set_data () =
      let item_key = Item.alloc @@ Item.of_bytes key in
      let item_value = Item.alloc @@ Item.of_bytes record in
      Cursor.set_key cursor_ptr item_key;
      Cursor.set_value cursor_ptr item_value
    in
    set_data ();
    Result.of_code (Cursor.insert cursor_ptr) ()
      "Couldn't insert data with the cursor"
  in
  get_cursor_ptr () >>= insert |> Result.get_ok_or_fail

let bulk_insert ~session ~tbl_name ~keys_and_records =
  assert (session != from_voidp Session.t null);
  let get_cursor_ptr () =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"bulk,raw"
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
            match (Char.code x, Char.code y) with
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
    keys_and_records |> List.sort_uniq cmp
    |> List.fold_left
         (fun acc (key, record) -> Result.bind acc (perform_write key record))
         (Result.Ok cursor_ptr)
  in
  let close_cursor cursor_ptr =
    Result.of_code (Cursor.close cursor_ptr) () "Couldn't close the cursor"
  in
  get_cursor_ptr () >>= perform_writes >>= close_cursor |> Result.get_ok_or_fail

let scan ~session ~tbl_name =
  assert (session != from_voidp Session.t null);
  let cursor_ptr =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
    |> Result.get_ok_or_fail
  in
  (* Set the minKey to 0, such that we always start from the first element in the table *)
  let minKey =
    let byte0 = CArray.make char ~initial:(Char.chr 0) 1 in
    Item.of_bytes (bigarray_of_array array1 Bigarray.Char byte0)
  in
  Cursor.set_key cursor_ptr (addr minKey);
  let cur_code =
    let code =
      let exact_ptr = allocate int 0 in
      let code = Cursor.search_near cursor_ptr exact_ptr in
      if !@exact_ptr < 0 then Cursor.next cursor_ptr else code
    in
    Stdlib.ref code
  in
  (* Allocate only one item and reuse it throughout the iterations *)
  let item_ptr = Item.alloc (make Item.t) in
  let next () =
    if cur_code.contents != 0 then None
    else
      let value =
        if Cursor.get_value cursor_ptr item_ptr != 0 then
          failwith "Couldn't get value from cursor";
        Item.to_bytes !@item_ptr
      in
      cur_code := Cursor.next cursor_ptr;
      Some value
  in
  next
