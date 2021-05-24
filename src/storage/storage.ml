let rec pow a = function
  | 0 ->
      1
  | 1 ->
      a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a


module ValueType = struct
  type t =
    | Integer
    | Numeric of int * int
    | Char of int
    | VarChar of int
    | Timestamp
end

module Value = struct
  type t =
    | Integer of int
    | Numeric of (int * int) * int
    (* TODO: support Char<2>("ab") and Varchar<20>("aoeaoe") *)
    | Char of string
    | VarChar of string
    | Timestamp of int

  let parse (t : ValueType.t) x =
    match t with
    | Integer ->
        Integer (int_of_string x)
    | Numeric (len, precision) ->
        let trimmed_x = String.trim x in
        let is_neg = trimmed_x.[0] = '-' in
        let chars =
          let char_list = List.of_seq (fun () -> String.to_seq trimmed_x ()) in
          match List.hd char_list with
          | '+' | '-' ->
              List.tl char_list
          | _ ->
              char_list
        in
        let build_val (digits_seen, digits_seen_fraction, is_fraction, result) x
            =
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
        let tmp = pow 10 precision in
        string_of_int (x / tmp) ^ "." ^ string_of_int (x mod tmp)
    | Char x ->
        x
    | VarChar x ->
        x
    | Timestamp x ->
        string_of_int x
end

module Schema = struct
  type column_name = string

  type t = (ValueType.t * column_name) list
end

module Tuple = struct
  type t = Value.t list

  let parse schema data =
    data
    |> String.split_on_char '|'
    |> List.map2 Value.parse (List.map fst schema)
end

module Table = struct
  type t =
    { name : String.t
    ; mutable schema : Schema.t
    ; mutable tuples : Tuple.t list
    }

  let create name schema = { name; schema; tuples = [] }

  let insert tbl tuple = tbl.tuples <- tuple :: tbl.tuples
end

module Database = struct
  type t = { mutable tables : Table.t list }

  let create () : t = { tables = [] }

  let insert db tbl = db.tables <- tbl :: db.tables
end
