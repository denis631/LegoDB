open Core

type t =
  | Integer
  | Numeric of int * int
  | Char of int
  | VarChar of int
  | Timestamp
[@@deriving compare, equal, hash, sexp, show { with_path = false }]

let sizeof = function
  | Integer -> 8 (* int64_t *)
  | Numeric _ -> 16 (* int64_t, int64_t *)
  | Char k -> k
  | VarChar k -> k
  | Timestamp -> 8 (* int64_t *)
