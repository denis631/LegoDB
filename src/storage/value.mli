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

val eq : t -> t -> bool
val parse : Value_type.t -> string -> t
val hash : t -> int64
val show : t -> string
