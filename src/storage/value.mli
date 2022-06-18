open Utils

type t =
  | Integer of int64
  | Numeric of (int * int) * int64
  (* TODO: support Char<2>("ab") and Varchar<20>("aoeaoe") *)
  | Char of string
  | VarChar of string
  | StringLiteral of string
  | Timestamp of int64
  | Bool of bool
[@@deriving hash, compare, equal, sexp]

val parse : Value_type.t -> string -> t
val show : t -> string
