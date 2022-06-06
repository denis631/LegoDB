open Utils

(* TODO: support aliasing and dot notation *)
type attr = AttrName of string | Star

(* TODO: support aliasing and dot notation *)
type tbl_name = string
type const = Int of int | Str of string
type pred = EqConst of attr * const | EqAttr of attr * attr
type tbl_elt = ColDef of string * Value_type.t
type ddl_expr = CreateTbl of tbl_name * tbl_elt list
type dml_expr = Select of attr list * tbl_name list * pred list option
type sql_expr = DML of dml_expr | DDL of ddl_expr

val show_attr : attr -> string
val show_const : const -> string
val show_pred : pred -> string
val show : sql_expr -> string
