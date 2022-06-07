open Utils

(* TODO: support aliasing and dot notation *)
type attr = AttrName of string | Star

(* TODO: support aliasing and dot notation *)
type tbl_name = string
type col_name = string
type path = string
type const = Int of int | Str of string
type pred = EqConst of attr * const | EqAttr of attr * attr
type key_type = PrimaryKey

type tbl_elt =
  | ColDef of string * Value_type.t
  | ConstraintDef of key_type * col_name list

type ddl_expr =
  | CreateTbl of tbl_name * tbl_elt list
  | DropTbl of tbl_name list

type dml_expr =
  | Select of attr list * tbl_name list * pred list option
  | Copy of tbl_name * path

type sql_expr = DML of dml_expr | DDL of ddl_expr

val show_attr : attr -> string
val show_const : const -> string
val show_pred : pred -> string
val show : sql_expr -> string
