type name = string

type t = {
  mutable db_ref : Wired_tiger.t;
  name : name;
  mutable schema : Schema.t;
}

type parent = t

module Iu = struct
  type t = name * Schema.column_name * Value_type.t

  let make name col t = (name, col, t)
  let eq a b = a = b
  let show (_, col, ty) = "col: " ^ col ^ " | type: " ^ Value_type.show ty
end

let marshal obj = Bytearray.marshal obj []
let unmarshal bytearray : Tuple.t = Bytearray.unmarshal bytearray 0

module Iter = struct
  type t = unit -> (Wired_tiger.bin_repr_t * Wired_tiger.bin_repr_t) option

  let make tbl = Wired_tiger.scan ~db:tbl.db_ref ~tbl_name:tbl.name

  let next iter =
    match iter () with
    | Some (_, value) -> (
        try Some (unmarshal value)
        with _ ->
          failwith "Data corruption happened. Cannot unmarshal the data")
    | None -> None
end

let name tbl = tbl.name
let schema tbl = tbl.schema
let create db_ref name schema = { db_ref; name; schema }

let insert tbl record =
  let key = [ List.nth record 2; List.nth record 1; List.nth record 0 ] in
  Wired_tiger.insert_record ~db:tbl.db_ref ~tbl_name:tbl.name ~key:(marshal key)
    ~record:(marshal [ Value.Integer 0L ])

let bulk_insert tbl records =
  (* TODO: implement proper to_key function *)
  let to_key record =
    [ List.nth record 2; List.nth record 1; List.nth record 0 ]
  in
  let map f list =
    let rec loop acc = function
      | [] -> List.rev acc
      | x :: xs -> loop (f x :: acc) xs
    in
    loop [] list
  in
  let data = map (fun r -> (marshal @@ to_key r, marshal r)) records in
  Wired_tiger.bulk_insert ~db:tbl.db_ref ~tbl_name:tbl.name
    ~keys_and_records:data

let ius tbl = List.map (fun (col, ty) -> Iu.make tbl.name col ty) tbl.schema
