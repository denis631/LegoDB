type t =
  | Integer
  | Numeric of int * int
  | Char of int
  | VarChar of int
  | Timestamp

let of_string = function
  | "integer" -> Integer
  | "numeric" -> Numeric (10, 10)
  | "char" -> Char 10
  | "varchar" -> VarChar 10
  | "timestamp" -> Timestamp
  | _ -> failwith "invalid string"

(* TODO: fix encoding *)
(* TODO: make a difference between encoding/decoding and show *)
let show = function
  | Integer -> "integer"
  | Numeric (_, _) ->
      "numeric"
      (* "numeric(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")" *)
  | Char _ -> "char" (* "char(" ^ string_of_int x ^ ")" *)
  | VarChar _ -> "varchar" (* "varchar(" ^ string_of_int x ^ ")" *)
  | Timestamp -> "timestamp"
