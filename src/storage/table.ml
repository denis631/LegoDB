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

module Iter = struct
  type t = unit -> (string * string) option

  let make tbl = Wired_tiger.scan ~db:tbl.db_ref ~tbl_name:tbl.name

  let next iter =
    match iter () with
    | Some (key, value) ->
        print_endline @@ "key: " ^ key ^ " | value: " ^ value;
        None
    | None -> None
end

let name tbl = tbl.name
let schema tbl = tbl.schema
let create db_ref name schema = { db_ref; name; schema }

let insert tbl _ =
  (* print_endline @@ Tuple.show record; *)
  Wired_tiger.insert_record ~db:tbl.db_ref ~tbl_name:tbl.name ~key:"custom key"
    ~record:"custom value"

let ius tbl = List.map (fun (col, ty) -> Iu.make tbl.name col ty) tbl.schema
