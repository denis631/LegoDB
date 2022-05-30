open Ctypes
open Foreign
open BatteriesExceptionless

module Result = struct
  include Result

  (* maps wiredtiger error code to result with a message *)
  let of_code code res msg =
    if code = 0 then Result.Ok res else Result.Error msg

  let get_ok_or_fail = function Ok x -> x | Error msg -> failwith msg
  let get_ok_or_none = function Ok x -> Some x | Error _ -> None

  module Infix = struct
    include Result.Infix

    let ( >>| ) t f = Result.map f t
  end
end

open Result.Infix

module C_Bindings = struct
  type session_t
  type connection_t
  type cursor_t
  type event_handler_t
  type item_t

  let session_t : session_t structure typ = structure "__wt_session"
  let connection_t : connection_t structure typ = structure "__wt_connection"

  let event_handler_t : event_handler_t structure typ =
    structure "__wt_event_handler"

  let cursor_t : cursor_t structure typ = structure "__wt_cursor"
  let item_t : item_t structure typ = structure "__wt_item"

  module Item = struct
    let data = field item_t "data" (ptr void)
    let size = field item_t "size" size_t
    let () = seal item_t
  end

  module Cursor = struct
    let _ = field cursor_t "session" (ptr session_t)
    let _ = field cursor_t "uri" Ctypes.string
    let _ = field cursor_t "key_format" Ctypes.string
    let _ = field cursor_t "value_format" Ctypes.string

    (* int __F(get_key)(WT_CURSOR *cursor, ...); *)
    let _ =
      field cursor_t "get_key"
        (funptr (ptr cursor_t @-> ptr item_t @-> returning int))

    (* int __F(get_value)(WT_CURSOR *cursor, ...); *)
    let get_value =
      field cursor_t "get_value"
        (funptr (ptr cursor_t @-> ptr item_t @-> returning int))

    (* void __F(set_key)(WT_CURSOR *cursor, ...); *)
    let set_key =
      field cursor_t "set_key"
        (funptr (ptr cursor_t @-> ptr item_t @-> returning void))

    (* void __F(set_value)(WT_CURSOR *cursor, ...); *)
    let set_value =
      field cursor_t "set_value"
        (funptr (ptr cursor_t @-> ptr item_t @-> returning void))

    (* int __F(compare)(WT_CURSOR *cursor, WT_CURSOR *other, int *comparep); *)
    let _ =
      field cursor_t "compare"
        (funptr (ptr cursor_t @-> ptr cursor_t @-> ptr int @-> returning int))

    (* int __F(equals)(WT_CURSOR *cursor, WT_CURSOR *other, int *equalp); *)
    let _ =
      field cursor_t "equals"
        (funptr (ptr cursor_t @-> ptr cursor_t @-> ptr int @-> returning int))

    (* int __F(next)(WT_CURSOR *cursor); *)
    let next = field cursor_t "next" (funptr (ptr cursor_t @-> returning int))

    (* int __F(prev)(WT_CURSOR *cursor); *)
    let _ = field cursor_t "prev" (funptr (ptr cursor_t @-> returning int))

    (* int __F(reset)(WT_CURSOR *cursor); *)
    let reset = field cursor_t "reset" (funptr (ptr cursor_t @-> returning int))

    (* int __F(search)(WT_CURSOR *cursor); *)
    let search =
      field cursor_t "search" (funptr (ptr cursor_t @-> returning int))

    (* int __F(search_near)(WT_CURSOR *cursor, int *exactp); *)
    let search_near =
      field cursor_t "search_near"
        (funptr (ptr cursor_t @-> ptr int @-> returning int))

    (* int __F(insert)(WT_CURSOR *cursor); *)
    let insert =
      field cursor_t "insert" (funptr (ptr cursor_t @-> returning int))

    let _ =
      field cursor_t "modify"
        (funptr (ptr cursor_t @-> ptr void @-> int @-> returning int))

    (* int __F(update)(WT_CURSOR *cursor); *)
    let _ = field cursor_t "update" (funptr (ptr cursor_t @-> returning int))

    (* int __F(remove)(WT_CURSOR *cursor); *)
    let _ = field cursor_t "remove" (funptr (ptr cursor_t @-> returning int))

    (* int __F(reserve)(WT_CURSOR *cursor); *)
    let _ = field cursor_t "reserve" (funptr (ptr cursor_t @-> returning int))

    (* uint64_t __F(checkpoint_id)(WT_CURSOR *cursor); *)
    let _ =
      field cursor_t "checkpoint_id"
        (funptr (ptr cursor_t @-> returning uint64_t))

    (* int __F(close)(WT_CURSOR *cursor); *)
    let close = field cursor_t "close" (funptr (ptr cursor_t @-> returning int))

    (* int __F(largest_key)(WT_CURSOR *cursor); *)
    let _ =
      field cursor_t "largest_key" (funptr (ptr cursor_t @-> returning int))

    (* int __F(reconfigure)(WT_CURSOR *cursor, const char *config); *)
    let _ =
      field cursor_t "reconfigure"
        (funptr (ptr cursor_t @-> Ctypes.string @-> returning int))

    (* int __F(bound)(WT_CURSOR *cursor, const char *config); *)
    let _ =
      field cursor_t "bound"
        (funptr (ptr cursor_t @-> Ctypes.string @-> returning int))

    (* int __F(cache)(WT_CURSOR *cursor); *)
    let _ = field cursor_t "cache" (funptr (ptr cursor_t @-> returning int))

    (* int __F(reopen)(WT_CURSOR *cursor, bool check_only); *)
    let _ =
      field cursor_t "reopen" (funptr (ptr cursor_t @-> bool @-> returning int))

    let _ = field cursor_t "uri_hash" uint64_t

    type q

    let q : q structure typ = structure "q"
    let _ = field q "tqe_next" (ptr cursor_t)
    let _ = field q "tqe_prev" (ptr (ptr cursor_t))
    let () = seal q
    let _ = field cursor_t "q" q
    let _ = field cursor_t "recno" uint64_t

    let _ =
      let wt_intpack64_maxsize = sizeof int64_t + 1 in
      field cursor_t "raw_recno_buf" (array wt_intpack64_maxsize uint8_t)

    let _ = field cursor_t "json_private" (ptr void)
    let _ = field cursor_t "lang_private" (ptr void)
    let _ = field cursor_t "key" item_t
    let _ = field cursor_t "value" item_t
    let _ = field cursor_t "saved_err" int
    let _ = field cursor_t "internal_uri" Ctypes.string
    let _ = field cursor_t "flags" uint32_t
    let () = seal cursor_t
  end

  module Session = struct
    (* static const WT_SESSION *)
    (*   stds = {NULL, NULL, __session_close, __session_reconfigure, __session_flush_tier, *)
    (*     __wt_session_strerror, __session_open_cursor, __session_alter, __session_create, *)
    (*     __wt_session_compact, __session_drop, __session_join, __session_log_flush, *)
    (*     __session_log_printf, __session_rename, __session_reset, __session_salvage, *)
    (*     __session_truncate, __session_upgrade, __session_verify, __session_begin_transaction, *)
    (*     __session_commit_transaction, __session_prepare_transaction, __session_rollback_transaction, *)
    (*     __session_query_timestamp, __session_timestamp_transaction, *)
    (*     __session_timestamp_transaction_uint, __session_checkpoint, __session_reset_snapshot, *)
    (*     __session_transaction_pinned_range, __session_get_rollback_reason, __wt_session_breakpoint}; *)

    let _ = field session_t "connection" (ptr connection_t)
    let _ = field session_t "app_private" (ptr void)

    (* int __F(close)(WT_SESSION *session, const char *config); *)
    let _ =
      field session_t "close"
        (funptr (ptr session_t @-> Ctypes.string @-> returning int))

    (* int __F(reconfigure)(WT_SESSION *session, const char *config); *)
    let _ =
      field session_t "reconfigure"
        (funptr (ptr session_t @-> Ctypes.string @-> returning int))

    (* int __F(flush_tier)(WT_SESSION *session, const char *config); *)
    let _ =
      field session_t "flush_tier"
        (funptr (ptr session_t @-> Ctypes.string @-> returning int))

    (* const char *__F(strerror)(WT_SESSION *session, int error); *)
    let _ =
      field session_t "strerror"
        (funptr
           (ptr session_t @-> Ctypes.string @-> int @-> returning Ctypes.string))

    (* int __F(open_cursor)(WT_SESSION *session,
                            const char *uri,
                            WT_HANDLE_NULLABLE(WT_CURSOR) *to_dup,
                            const char *config,
                            WT_CURSOR **cursorp); *)
    let open_cursor =
      field session_t "open_cursor"
        (funptr
           (ptr session_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
           @-> ptr (ptr cursor_t)
           @-> returning int))

    (* int __F(alter)(WT_SESSION *session,
                      const char *name,
                      const char *config); *)
    let _ =
      field session_t "alter"
        (funptr
           (ptr session_t @-> Ctypes.string @-> Ctypes.string @-> returning int))

    (* int __F(create)(WT_SESSION *session,
                       const char *name,
                       const char *config); *)
    let create =
      field session_t "create"
        (funptr
           (ptr session_t @-> Ctypes.string @-> Ctypes.string @-> returning int))

    let () = seal session_t
  end

  module Connection = struct
    (* static const WT_CONNECTION stdc = {__conn_close, __conn_debug_info, __conn_reconfigure, *)
    (*   __conn_get_home, __conn_configure_method, __conn_is_new, __conn_open_session, *)
    (*   __conn_query_timestamp, __conn_set_timestamp, __conn_rollback_to_stable, *)
    (*   __conn_load_extension, __conn_add_data_source, __conn_add_collator, __conn_add_compressor, *)
    (*   __conn_add_encryptor, __conn_add_extractor, __conn_set_file_system, __conn_add_storage_source, *)
    (*   __conn_get_storage_source, __conn_get_extension_api} *)

    (* int __F(close)(WT_CONNECTION *connection, const char *config); *)
    let _ =
      field connection_t "close"
        (funptr (ptr connection_t @-> Ctypes.string @-> returning int))

    (* int __F(conn_debug_info)(WT_CONNECTION *wt_conn, const char *config); *)
    let _ =
      field connection_t "debug_info"
        (funptr (ptr connection_t @-> Ctypes.string @-> returning int))

    (* int __F(reconfigure)(WT_CONNECTION *connection, const char *config); *)
    let _ =
      field connection_t "reconfigure"
        (funptr (ptr connection_t @-> Ctypes.string @-> returning int))

    (* const char *__F(get_home)(WT_CONNECTION *connection); *)
    let _ =
      field connection_t "get_home"
        (funptr (ptr connection_t @-> returning Ctypes.string))

    (* int __F(configure_method)(WT_CONNECTION *connection,
                                 const char *method,
                                 const char *uri,
                                 const char *config,
                                 const char *type,
                                 const char *check); *)
    let _ =
      field connection_t "configure_method"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> Ctypes.string
          @-> Ctypes.string @-> Ctypes.string @-> Ctypes.string
          @-> returning int))

    (* int __F(is_new)(WT_CONNECTION *connection); *)
    let _ =
      field connection_t "is_new" (funptr (ptr connection_t @-> returning int))

    (* int __F(open_session)(WT_CONNECTION *connection,
                             WT_EVENT_HANDLER *event_handler,
                             const char *config,
                             WT_SESSION **sessionp); *)
    let open_session =
      field connection_t "open_session"
        (funptr
           (ptr connection_t @-> ptr event_handler_t @-> Ctypes.string
           @-> ptr (ptr session_t)
           @-> returning int))

    (* int __F(query_timestamp)(WT_CONNECTION *connection, char *hex_timestamp, const char *config); *)
    let _ =
      field connection_t "query_timestamp"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> Ctypes.string
          @-> returning int))

    (* int __F(set_timestamp)(WT_CONNECTION *connection, const char *config); *)
    let _ =
      field connection_t "set_timestamp"
        (funptr (ptr connection_t @-> Ctypes.string @-> returning int))

    (* int __F(rollback_to_stable)(WT_CONNECTION *connection, const char *config); *)
    let _ =
      field connection_t "rollback_to_stable"
        (funptr (ptr connection_t @-> Ctypes.string @-> returning int))

    (* int __F(load_extension)(WT_CONNECTION *connection, const char *path, const char *config); *)
    let _ =
      field connection_t "load_extension"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> Ctypes.string
          @-> returning int))

    (* int __F(add_data_source)(WT_CONNECTION *connection,
                                const char *prefix,
                                WT_DATA_SOURCE *data_source,
                                const char *config); *)
    let _ =
      field connection_t "add_data_source"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
          @-> returning int))

    (* int __F(add_collator)(WT_CONNECTION *connection,
                             const char *name,
                             WT_COLLATOR *collator,
                             const char *config); *)
    let _ =
      field connection_t "add_collator"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
          @-> returning int))

    (* int __F(add_compressor)(WT_CONNECTION *connection,
                               const char *name,
                               WT_COMPRESSOR *compressor,
                               const char *config); *)
    let _ =
      field connection_t "add_compressor"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
          @-> returning int))

    (* int __F(add_encryptor)(WT_CONNECTION *connection,
                              const char *name,
                              WT_ENCRYPTOR *encryptor,
                              const char *config); *)
    let _ =
      field connection_t "add_encryptor"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
          @-> returning int))

    (* int __F(add_extractor)(WT_CONNECTION *connection,
                              const char *name,
                              WT_EXTRACTOR *extractor,
                              const char *config); *)
    let _ =
      field connection_t "add_extractor"
        (funptr
           (ptr connection_t @-> Ctypes.string @-> ptr void @-> Ctypes.string
          @-> returning int))

    (* int __F(set_file_system)(WT_CONNECTION *connection, WT_FILE_SYSTEM *fs, const char *config); *)
    let _ =
      field connection_t "set_file_system"
        (funptr
           (ptr connection_t @-> ptr void @-> Ctypes.string @-> returning int))

    let () = seal connection_t
  end

  module WiredTiger = struct
    (* int wiredtiger_open(const char *home,
                           WT_EVENT_HANDLER *event_handler,
                           const char *config,
                           WT_CONNECTION **connectionp) *)
    let wiredtiger_open =
      foreign "wiredtiger_open"
        (Ctypes.string @-> ptr void @-> Ctypes.string
        @-> ptr (ptr connection_t)
        @-> returning int)
  end
end

type session_t = C_Bindings.session_t structure
type t = session_t ptr
type bin_repr_t = Bytearray.t

let connection_t = C_Bindings.connection_t
let session_t = C_Bindings.session_t
let event_handler_t = C_Bindings.event_handler_t
let cursor_t = C_Bindings.cursor_t
let item_t = C_Bindings.item_t

module IsolationLevelConfig = struct
  type t = Snapshot | ReadCommitted

  let show = function
    | Snapshot -> "isolation=snapshot"
    | ReadCommitted -> "isolation=read_committed"
end

module Session = struct
  let open_cursor session_ptr =
    getf !@session_ptr C_Bindings.Session.open_cursor session_ptr

  let create_tbl session_ptr =
    getf !@session_ptr C_Bindings.Session.create session_ptr
end

module Cursor = struct
  let get_value cursor_ptr =
    getf !@cursor_ptr C_Bindings.Cursor.get_value cursor_ptr

  let set_key cursor_ptr =
    getf !@cursor_ptr C_Bindings.Cursor.set_key cursor_ptr

  let set_value cursor_ptr =
    getf !@cursor_ptr C_Bindings.Cursor.set_value cursor_ptr

  let insert cursor_ptr = getf !@cursor_ptr C_Bindings.Cursor.insert cursor_ptr
  let next cursor_ptr = getf !@cursor_ptr C_Bindings.Cursor.next cursor_ptr
  let search cursor_ptr = getf !@cursor_ptr C_Bindings.Cursor.search cursor_ptr

  let search_near cursor_ptr =
    getf !@cursor_ptr C_Bindings.Cursor.search_near cursor_ptr

  let close cursor_ptr = getf !@cursor_ptr C_Bindings.Cursor.close cursor_ptr
  let reset cursor_ptr = getf !@cursor_ptr C_Bindings.Cursor.reset cursor_ptr
end

module Item = struct
  let get_data item = getf item C_Bindings.Item.data
  let get_size item = getf item C_Bindings.Item.size
  let set_data item = setf item C_Bindings.Item.data
  let set_size item = setf item C_Bindings.Item.size

  let of_bytes data =
    let item = make item_t in
    set_data item (bigarray_start array1 data |> to_voidp);
    set_size item (Unsigned.Size_t.of_int @@ Bytearray.length data);
    item

  let to_bytes item =
    let data = get_data item |> from_voidp char in
    let size = get_size item |> Unsigned.Size_t.to_int in
    bigarray_of_ptr array1 size Bigarray.char data
end

(* Establishes a connection and creates a new session *)
let init_and_open_session ~path ~config ~isolation_config =
  let open_connection () =
    let conn_ptr_ptr =
      allocate (ptr connection_t) (from_voidp connection_t null)
    in
    Result.of_code
      (C_Bindings.WiredTiger.wiredtiger_open path null config conn_ptr_ptr)
      conn_ptr_ptr "Couldn't create the database"
  in
  let open_session conn_ptr_ptr =
    assert (not (is_null !@conn_ptr_ptr));
    let conn_ptr = !@conn_ptr_ptr in
    let session_ptr_ptr =
      allocate (ptr session_t) (from_voidp session_t null)
    in
    let open_session_f = !@(conn_ptr |-> C_Bindings.Connection.open_session) in
    let code =
      open_session_f conn_ptr
        (from_voidp event_handler_t null)
        (IsolationLevelConfig.show isolation_config)
        session_ptr_ptr
    in
    Result.of_code code session_ptr_ptr "Couldn't create the session"
  in
  let get_session_ptr session_ptr_ptr =
    assert (not (is_null !@session_ptr_ptr));
    !@session_ptr_ptr
  in
  open_connection () >>= open_session >>| get_session_ptr
  |> Result.get_ok_or_fail

let create_tbl ~session ~tbl_name ~config =
  assert (session != from_voidp session_t null);
  if Session.create_tbl session ("table:" ^ tbl_name) config != 0 then
    failwith "Couldn't create the session"

let open_tbl_cursor ~session_ptr ~tbl_name ~config =
  let open_cursor () =
    assert (session_ptr != from_voidp session_t null);
    let cursor_ptr_ptr = allocate (ptr cursor_t) (from_voidp cursor_t null) in
    let code =
      Session.open_cursor session_ptr ("table:" ^ tbl_name) null config
        cursor_ptr_ptr
    in
    Result.of_code code cursor_ptr_ptr "Couldn't open a cursor"
  in
  let get_cursor_ptr cursor_ptr_ptr =
    assert (not (is_null !@cursor_ptr_ptr));
    !@cursor_ptr_ptr
  in
  open_cursor () >>| get_cursor_ptr

let lookup_one ~session ~tbl_name ~key =
  assert (session != from_voidp session_t null);
  let get_cursor_ptr () =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
  in
  let search_for_key cursor_ptr =
    Cursor.set_key cursor_ptr (allocate item_t (Item.of_bytes key));
    Result.of_code (Cursor.search cursor_ptr) cursor_ptr "Nothing found"
  in
  let get_value cursor_ptr =
    let item_ptr = allocate item_t (make item_t) in
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
  assert (session != from_voidp session_t null);
  let get_cursor_ptr () =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
  in
  let insert cursor_ptr =
    let set_data () =
      let item_key = allocate item_t @@ Item.of_bytes key in
      let item_value = allocate item_t @@ Item.of_bytes record in
      Cursor.set_key cursor_ptr item_key;
      Cursor.set_value cursor_ptr item_value
    in
    set_data ();
    Result.of_code (Cursor.insert cursor_ptr) ()
      "Couldn't insert data with the cursor"
  in
  get_cursor_ptr () >>= insert |> Result.get_ok_or_fail

let bulk_insert ~session ~tbl_name ~keys_and_records =
  assert (session != from_voidp session_t null);
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
  assert (session != from_voidp session_t null);
  let cursor_ptr =
    open_tbl_cursor ~session_ptr:session ~tbl_name ~config:"raw"
    |> Result.get_ok_or_fail
  in
  (* Set the minKey to 0, such that we always start from the first element in the table *)
  let minKey =
    let byte0 = CArray.make char 1 in
    CArray.set byte0 0 (Char.chr 0);
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
  let item_ptr = allocate item_t (make item_t) in
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
