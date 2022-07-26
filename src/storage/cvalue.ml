open Utils
open Core

type t = Int of int64 | String of string

let ty = function
  | Int _ -> Value_type.Integer
  | String s -> Value_type.VarChar (String.length s)

let parse_and_write (ty : Value_type.t) x write =
  match ty with
  | Integer -> write (Int (Int64.of_string x))
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
      write (Int int64_val)
  | Char k ->
      assert (Int.equal k @@ String.length x);
      write (String x)
  | VarChar k ->
      write (String x);

      (* append nulls if needed *)
      (* TODO: no need to write nulls? can just move the iterator forward? *)
      let l = String.length x in
      let nulls_length = k - l in
      if Int.(nulls_length > 0) then
        let str = String.make nulls_length @@ Char.of_int_exn 0 in
        write (String str)
  | Timestamp -> write (Int (Int64.of_string x))
