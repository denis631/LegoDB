open Frontend
open Storage

val find_table : Database.t -> Ast.tbl -> Table.t

val find_column_attr : Database.t -> string -> Table.Iu.t
