open Core

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

let equal a b =
  match (a, b) with
  | Integer x, Integer y -> Int64.(x = y)
  | Numeric ((l1, prec1), x), Numeric ((l2, prec2), y) ->
      l1 = l2 && prec1 = prec2 && Int64.(x = y)
  | Char c1, Char c2 -> String.equal c1 c2
  | VarChar c1, VarChar c2 -> String.equal c1 c2
  | Char c, StringLiteral x -> String.equal x c
  | VarChar c, StringLiteral x -> String.equal x c
  | Timestamp x, Timestamp y -> Int64.(x = y)
  | Bool a, Bool b -> Bool.(a = b)
  | _ -> false

