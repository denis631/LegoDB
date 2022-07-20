open Ctypes
open Foreign

module Index = struct
   include Index
end

module Schema = struct
   include Schema
end

module TableMeta = struct
   include Table_meta
end

module Value_type = struct
  include Value_type
end

module Order = struct
  type direction = Ascending | Descending [@@deriving show]
end

(* Helper functions *)

let tap f x =
  f x;
  x

let ( <% ) f g x = f (g x)
let ( %> ) f g x = g (f x)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

let memcpy =
  foreign "memcpy" (ptr char @-> ptr char @-> size_t @-> returning (ptr char))

let memcpy_ocaml_string =
  foreign "memcpy"
    (ptr char @-> ocaml_string @-> size_t @-> returning (ptr char))
