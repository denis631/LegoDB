type name = string

type t =
  { name : name
  ; mutable schema : Schema.t
  ; mutable tuples : Tuple.t BatVect.t
  }

type parent = t

module Iu = struct
  type t = name * Schema.column_name * Value_type.t

  let make name col t = (name, col, t)

  let eq a b = a = b

  let show (_, col, ty) = "col: " ^ col ^ " | type: " ^ Value_type.show ty
end

module Iter = struct
  type t =
    { tuples : Tuple.t BatVect.t
    ; mutable idx : int
    }

  let make (tbl : parent) = { tuples = tbl.tuples; idx = 0 }

  let next iter =
    match BatVect.length iter.tuples with
    | l when iter.idx < l ->
        let res = Some (BatVect.get iter.tuples iter.idx) in
        iter.idx <- iter.idx + 1 ;
        res
    | _ ->
        None
end

let name tbl = tbl.name

let schema tbl = tbl.schema

let create name schema = { name; schema; tuples = BatVect.empty }

let insert tbl tuple = tbl.tuples <- BatVect.append tuple tbl.tuples

let ius tbl = List.map (fun (col, ty) -> Iu.make tbl.name col ty) tbl.schema
