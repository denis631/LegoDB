open Ctypes

type t = Bindings.session_t structure

let t = Bindings.session_t
let alloc_ptr () = allocate (ptr t) (from_voidp t null)

let open_cursor session_ptr =
  getf !@session_ptr Bindings.Session.open_cursor session_ptr

let create_tbl session_ptr =
  getf !@session_ptr Bindings.Session.create session_ptr

let begin_txn session_ptr =
  getf !@session_ptr Bindings.Session.begin_transaction session_ptr

let commit_txn session_ptr =
  getf !@session_ptr Bindings.Session.commit_transaction session_ptr

let rollback_txn session_ptr =
  getf !@session_ptr Bindings.Session.rollback_transaction session_ptr
