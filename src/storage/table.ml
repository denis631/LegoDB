type name = string

type t =
  { name : name
  ; mutable schema : Schema.t
  ; mutable tuples : Tuple.t list
  }

type iu = name * Schema.column_name * Value_type.t

let create name schema = { name; schema; tuples = [] }

let insert tbl tuple = tbl.tuples <- tuple :: tbl.tuples

let ius tbl = List.map (fun (col, ty) -> (tbl.name, col, ty)) tbl.schema
