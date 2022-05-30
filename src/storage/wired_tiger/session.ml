open Ctypes

let open_cursor session_ptr =
  getf !@session_ptr Bindings.Session.open_cursor session_ptr

let create_tbl session_ptr =
  getf !@session_ptr Bindings.Session.create session_ptr
