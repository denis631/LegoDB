open Frontend
open Storage

val find_table : Database.t -> Ast.tbl -> Table.RegularTbl.Meta.t
val find_column_attr : Database.t -> string -> Table.RegularTbl.Iu.t
