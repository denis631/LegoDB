open Ctypes

let t = Bindings.cursor_t
let alloc_ptr () = allocate (ptr t) (from_voidp t null)

let get_value cursor_ptr =
  getf !@cursor_ptr Bindings.Cursor.get_value cursor_ptr

let set_key cursor_ptr = getf !@cursor_ptr Bindings.Cursor.set_key cursor_ptr

let set_value cursor_ptr =
  getf !@cursor_ptr Bindings.Cursor.set_value cursor_ptr

let insert cursor_ptr = getf !@cursor_ptr Bindings.Cursor.insert cursor_ptr
let remove cursor_ptr = getf !@cursor_ptr Bindings.Cursor.remove cursor_ptr
let next cursor_ptr = getf !@cursor_ptr Bindings.Cursor.next cursor_ptr
let search cursor_ptr = getf !@cursor_ptr Bindings.Cursor.search cursor_ptr

let search_near cursor_ptr =
  getf !@cursor_ptr Bindings.Cursor.search_near cursor_ptr

let close cursor_ptr = getf !@cursor_ptr Bindings.Cursor.close cursor_ptr
let reset cursor_ptr = getf !@cursor_ptr Bindings.Cursor.reset cursor_ptr
