open BatteriesExceptionless

type t =
  | Integer of int
  | Numeric of (int * int) * int
  | Char of string
  | VarChar of string
  | StringLiteral of string
  | Timestamp of int
  | Bool of bool


let eq a b =
  match (a, b) with
  | Integer x, Integer y ->
      x = y
  | Numeric ((l1, prec1), x), Numeric ((l2, prec2), y) ->
      l1 = l2 && prec1 = prec2 && x = y
  | Char c1, Char c2 ->
      String.equal c1 c2
  | VarChar c1, VarChar c2 ->
      String.equal c1 c2
  | Char c, StringLiteral x ->
      String.equal x c
  | VarChar c, StringLiteral x ->
      String.equal x c
  | Timestamp x, Timestamp y ->
      x = y
  | Bool a, Bool b ->
      a = b
  | _ ->
      false


let parse (t : Value_type.t) x =
  match t with
  | Integer ->
      Integer (int_of_string x)
  | Numeric (len, precision) ->
      let trimmed_x = String.trim x in
      let is_neg = trimmed_x.[0] = '-' in
      let chars =
        let char_list = String.to_list trimmed_x in
        match List.first char_list with
        | '+' | '-' ->
            Option.get @@ List.tl char_list
        | _ ->
            char_list
      in
      let build_val (digits_seen, digits_seen_fraction, is_fraction, result) x =
        if x = '.'
        then
          if is_fraction
          then failwith "invalid number format: already in fraction"
          else (digits_seen, digits_seen_fraction, true, result)
        else
          let zero = Char.code '0' in
          let nine = Char.code '9' in
          let cur = Char.code x in
          if cur >= zero && cur <= nine
          then
            if is_fraction
            then
              ( digits_seen
              , digits_seen_fraction + 1
              , is_fraction
              , (result * 10) + cur - zero )
            else
              ( digits_seen + 1
              , digits_seen_fraction
              , is_fraction
              , (result * 10) + cur - zero )
          else
            failwith
              "invalid number format: invalid character in numeric string"
      in
      let digits_seen, digits_seen_fraction, _, numeric_val =
        List.fold_left build_val (0, 0, false, 0) chars
      in
      if digits_seen > len || digits_seen_fraction > precision
      then failwith "invalid number format: loosing precision" ;
      Numeric ((len, precision), if is_neg then -numeric_val else numeric_val)
  (* TODO: add tests for parsing *)
  | Char _ ->
      Char x
  | VarChar _ ->
      VarChar x
  | Timestamp ->
      Timestamp (int_of_string x)


let show = function
  | Integer x ->
      string_of_int x
  | Numeric ((_, precision), x) ->
      let tmp = Int.pow  10 precision in
      string_of_int (x / tmp) ^ "." ^ string_of_int (x mod tmp)
  | Char x ->
      x
  | VarChar x ->
      x
  | StringLiteral x ->
      x
  | Timestamp x ->
      string_of_int x
  | Bool x ->
      string_of_bool x
