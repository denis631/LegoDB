open Frontend
open Storage

val find_table : Database.t -> Ast.tbl -> Table.T.Meta.t
val find_column_attr : Database.t -> string -> Table.T.Iu.t
