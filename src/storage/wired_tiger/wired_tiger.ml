open Ctypes
open Foreign

module C_Bindings = struct
  type session_t
  type connection_t
  type cursor_t
  type event_handler_t

  let session_t : session_t structure typ = structure "__wt_session"
  let connection_t : connection_t structure typ = structure "__wt_connection"
  let cursor_t : cursor_t structure typ = structure "__wt_cursor"

  let event_handler_t : event_handler_t structure typ =
    structure "__wt_event_handler"

  (* module Item = struct *)
  (*   type item_t *)

  (*   let item_t : item_t structure typ = structure "__wt_item" *)

  (*   let data = field item_t "data" (ptr void) *)

  (*   let size = field item_t "size" size_t *)

  (*   let () = seal item_t *)
  (* end *)

  module Cursor = struct
    (*   (\* void __F(set_key)(WT_CURSOR *cursor, ...); *\) *)
    (*   let set_key = *)
    (*     field cursor_t "set_key" *)
    (*       (funptr (ptr cursor_t @-> ptr Item.item_t @-> returning void)) *)

    (*   (\* void __F(set_value)(WT_CURSOR *cursor, ...); *\) *)
    (*   let set_value = *)
    (*     field cursor_t "set_value" *)
    (*       (funptr (ptr cursor_t @-> ptr Item.item_t @-> returning void)) *)

    (*   (\* int __F(insert)(WT_CURSOR *cursor); *\) *)
    (*   let insert = *)
    (*     field cursor_t "insert" (funptr (ptr cursor_t @-> returning int)) *)

    (*   let () = seal cursor_t *)
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
    let _ =
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

type connection_t = C_Bindings.connection_t structure
type session_t = C_Bindings.session_t structure

let connection_t = C_Bindings.connection_t
let session_t = C_Bindings.session_t

(* let cursor_t = C_Bindings.cursor_t *)
let event_handler_t = C_Bindings.event_handler_t

(* let cursor_t = C_Bindings.Cursor.cursor_t *)
(* let item_t = C_Bindings.Item.item_t *)

type t = connection_t ptr * session_t ptr

module IsolationLevelConfig = struct
  type t = Snapshot | ReadCommitted

  let show = function
    | Snapshot -> "isolation=snapshot"
    | ReadCommitted -> "isolation=read_committed"
end

module TblCreationConfig = struct
  type t = string
end

(* let print_ptr ptr str = *)
(*   ptr |> to_voidp |> raw_address_of_ptr *)
(*   |> Printf.printf "Address of %s is: 0x%nx\n" str *)

let init ~path ~config =
  let conn_ptr_ptr =
    allocate (ptr connection_t) (from_voidp connection_t null)
  in
  let code =
    C_Bindings.WiredTiger.wiredtiger_open path null config conn_ptr_ptr
  in
  if code != 0 then failwith "Couldn't create the database";
  assert (not (is_null !@conn_ptr_ptr));
  (!@conn_ptr_ptr, from_voidp session_t null)

let open_session ~db ~config =
  let conn_ptr = fst db in
  assert (not (is_null conn_ptr));
  let session_ptr_ptr = allocate (ptr session_t) (from_voidp session_t null) in
  let open_session_f = !@(conn_ptr |-> C_Bindings.Connection.open_session) in
  let code =
    open_session_f conn_ptr
      (from_voidp event_handler_t null)
      (IsolationLevelConfig.show config)
      session_ptr_ptr
  in
  if code != 0 then failwith "Couldn't create the session";
  assert (not (is_null !@session_ptr_ptr));
  (conn_ptr, !@session_ptr_ptr)

type table_name = string

let create_tbl ~db ~tbl_name ~config =
  let session_ptr = snd db in
  assert (session_ptr != from_voidp session_t null);
  let create_tbl_f = !@(session_ptr |-> C_Bindings.Session.create) in
  print_endline "managed to be here";
  let code = create_tbl_f session_ptr ("table:" ^ tbl_name) config in
  if code != 0 then failwith "Couldn't create the session"

(* TODO: implement creating a item from data, the size field should be calculated based on data length *)
(* let make_item data = *)
(*   make item_t *)

(* let insert_record ~db ~tbl_name ~key ~record = *)
(*   let session_ptr = snd db in *)
(*   assert (session_ptr != from_voidp session_t null); *)
(*   (\* Open a cursor if needed *\) *)
(*   (\* TODO: cache the cursors, or maybe cache them in a batch insert? *\) *)
(*   let cursor_ptr_ptr = allocate (ptr cursor_t) (from_voidp cursor_t null) in *)
(*   let open_cursor_f = getf !@session_ptr C_Bindings.Session.open_cursor in *)
(*   let code = open_cursor_f session_ptr tbl_name null "raw" cursor_ptr_ptr in *)
(*   if code != 0 then failwith "Couldn't open a cursor"; *)

(*   (\* TODO: set key and payload item *\) *)
(*   let cursor_ptr = !@cursor_ptr_ptr in *)
(*   let set_key_f = getf !@cursor_ptr C_Bindings.Cursor.set_key in *)
(*   let set_value_f = getf !@cursor_ptr C_Bindings.Cursor.set_value in *)
(*   set_key_f cursor_ptr (from_voidp item_t null); *)
(*   set_value_f cursor_ptr (from_voidp item_t null); *)

(*   (\* perform cursor insert *\) *)
(*   let insert_f = getf !@cursor_ptr C_Bindings.Cursor.insert in *)
(*   let code = insert_f cursor_ptr in *)
(*   if code != 0 then failwith "Couldn't insert data with the cursor"; *)
