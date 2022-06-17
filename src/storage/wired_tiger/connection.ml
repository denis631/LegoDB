open Ctypes

let t = Bindings.connection_t
let alloc_ptr () = allocate (ptr t) (from_voidp t null)

let open_session conn_ptr =
  getf !@conn_ptr Bindings.Connection.open_session conn_ptr
