open Ctypes
open Foreign

module type Marshaller = sig
  type t
  type v

  val marshal : t -> v
  val unmarshal : v -> t
end

module Value_type = struct
  include Value_type
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
