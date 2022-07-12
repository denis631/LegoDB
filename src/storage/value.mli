open Utils

(* TODO: used only for match currently *)
type t =
  | Integer of int64
  | Numeric of (int * int) * int64
  (* TODO: support Char<2>("ab") and Varchar<20>("aoeaoe") *)
  | Char of string
  | VarChar of string
  | StringLiteral of string
  | Timestamp of int64
  | Bool of bool
[@@deriving hash, compare, equal, sexp, show]

module C : sig
  type t = Int of int64 | String of string

  val ty : t -> Value_type.t
end

val parse_and_write : Value_type.t -> string -> (C.t -> unit) -> unit
