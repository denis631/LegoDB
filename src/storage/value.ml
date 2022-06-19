open Utils
open Core

type t =
  | Integer of int64
  | Numeric of (int * int) * int64
  | Char of string
  | VarChar of string
  (* TODO: this is not really needed if analysis is done correctly -> Can create a VarChar or Char *)
  | StringLiteral of string
  | Timestamp of int64
  | Bool of bool
[@@deriving hash, compare, sexp]

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

(* TODO: add tests for parsing *)
let parse (t : Value_type.t) x =
  match t with
  | Integer -> Integer (Int64.of_string x)
  | Numeric (len, precision) ->
      let trimmed_x = x in
      let is_neg = Char.equal '-' @@ String.get x 0 in
      let chars =
        let char_list = String.to_list trimmed_x in
        match List.hd_exn char_list with
        | '+' | '-' -> List.tl_exn char_list
        | _ -> char_list
      in
      let build_val (digits_seen, digits_seen_fraction, is_fraction, result) x =
        if Char.equal x '.' then
          if is_fraction then
            failwith "invalid number format: already in fraction"
          else (digits_seen, digits_seen_fraction, true, result)
        else
          let zero = Char.to_int '0' in
          let nine = Char.to_int '9' in
          let cur = Char.to_int x in
          if cur >= zero && cur <= nine then
            if is_fraction then
              ( digits_seen,
                digits_seen_fraction + 1,
                is_fraction,
                (result * 10) + cur - zero )
            else
              ( digits_seen + 1,
                digits_seen_fraction,
                is_fraction,
                (result * 10) + cur - zero )
          else
            failwith
              "invalid number format: invalid character in numeric string"
      in
      let digits_seen, digits_seen_fraction, _, numeric_val =
        List.fold_left ~f:build_val ~init:(0, 0, false, 0) chars
      in
      if digits_seen > len || digits_seen_fraction > precision then
        failwith "invalid number format: loosing precision";
      let int64_val = Int64.of_int numeric_val in
      Numeric
        ((len, precision), if is_neg then Int64.neg int64_val else int64_val)
  | Char _ -> Char x
  | VarChar _ -> VarChar x
  | Timestamp -> Timestamp (Int64.of_string x)

let show = function
  | Integer x -> Int64.to_string x
  | Numeric ((_, precision), x) ->
      let tmp = Int64.pow (Int64.of_int 10) (Int64.of_int precision) in
      Int64.to_string Int64.(x / tmp) ^ "." ^ Int64.to_string Int64.(x % tmp)
  | Char x -> x
  | VarChar x -> x
  | StringLiteral x -> x
  | Timestamp x -> Int64.to_string x
  | Bool x -> string_of_bool x
