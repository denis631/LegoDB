type name = string

type t =
  { name : name
  ; mutable schema : Schema.t
  ; mutable tuples : Tuple.t list
  }

let create name schema = { name; schema; tuples = [] }

let insert tbl tuple = tbl.tuples <- tuple :: tbl.tuples
