open Core

type t =
  | Integer
  | Numeric of int * int
  | Char of int
  | VarChar of int
  | Timestamp
[@@deriving sexp, show]

let of_string = function
  | "integer" -> Integer
  | "numeric" -> Numeric (10, 10)
  | "char" -> Char 10
  | "varchar" -> VarChar 10
  | "timestamp" -> Timestamp
  | _ -> failwith "invalid string"
