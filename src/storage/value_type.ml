type t =
  | Integer
  | Numeric of int * int
  | Char of int
  | VarChar of int
  | Timestamp

let show = function
  | Integer ->
      "integer"
  | Numeric (a, b) ->
      "numeric(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
  | Char x ->
      "char(" ^ string_of_int x ^ ")"
  | VarChar x ->
      "varchar(" ^ string_of_int x ^ ")"
  | Timestamp ->
      "timestamp"
