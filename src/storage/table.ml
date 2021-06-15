type name = string

type t =
  { name : name
  ; mutable schema : Schema.t
  ; mutable tuples : Tuple.t BatVect.t
  }

module Iu = struct
  type t = name * Schema.column_name * Value_type.t

  let make name col t = (name, col, t)

  let eq a b = a = b
end

let name tbl = tbl.name

let schema tbl = tbl.schema

let tuple_at_idx tbl idx =
  match BatVect.length tbl.tuples with
  | l when idx < l ->
      Some (BatVect.get tbl.tuples idx)
  | _ ->
      None


let create name schema = { name; schema; tuples = BatVect.empty }

let insert tbl tuple = tbl.tuples <- BatVect.append tuple tbl.tuples

let ius tbl = List.map (fun (col, ty) -> Iu.make tbl.name col ty) tbl.schema
