type t =
  | Integer of int
  | Numeric of (int * int) * int
  (* TODO: support Char<2>("ab") and Varchar<20>("aoeaoe") *)
  | Char of string
  | VarChar of string
  | StringLiteral of string
  | Timestamp of int
  | Bool of bool


val eq: t -> t -> bool

val parse: Value_type.t -> string -> t

val show: t -> string
