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

let ( %> ) f g x = g (f x)

let curry f x y = f (x,y)
