type t = { mutable tables : Table.t list }

let create () : t = { tables = [] }

let tbls t = t.tables

let insert db tbl = db.tables <- tbl :: db.tables
