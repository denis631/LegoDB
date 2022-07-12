open Utils
open Core

type t =
  | Integer of int64
  | Numeric of (int * int) * int64
  | Char of string
  | VarChar of string
  | StringLiteral of string
  | Timestamp of int64
  | Bool of bool
[@@deriving hash, compare, sexp, show]

module C = struct
  type t = Int of int64 | String of string

  let ty = function
    | Int _ -> Value_type.Integer
    | String s -> Value_type.VarChar (String.length s)
end

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

let parse_and_write (ty : Value_type.t) x write =
  match ty with
  | Integer -> write (C.Int (Int64.of_string x))
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
      let int64_val =
        let x = Int64.of_int numeric_val in
        if is_neg then Int64.neg x else x
      in
      write (C.Int int64_val)
  | Char k ->
      assert (Int.equal k @@ String.length x);
      write (C.String x)
  | VarChar k ->
      write (C.String x);

      (* append nulls if needed *)
      let l = String.length x in
      let nulls_length = k - l in
      if Int.(nulls_length > 0) then
        let str = String.make nulls_length @@ Char.of_int_exn 0 in
        write (C.String str)
  | Timestamp -> write (C.Int (Int64.of_string x))
