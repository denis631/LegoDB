open Ctypes
open Foreign

let memcpy =
  foreign "memcpy" (ptr char @-> ptr char @-> size_t @-> returning (ptr char))

let memcpy_ocaml_string =
  foreign "memcpy"
    (ptr char @-> ocaml_string @-> size_t @-> returning (ptr char))
