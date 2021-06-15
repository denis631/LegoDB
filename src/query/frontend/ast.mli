(* TODO: support aliasing and dot notation *)
type attr =
  | AttrName of string
  | Star

(* TODO: support aliasing and dot notation *)
type tbl = TblName of string

type const =
  | Int of int
  | Str of string

type pred =
  | EqConst of attr * const
  | EqAttr of attr * attr

type sql_expr = Select of attr list * tbl list * pred list option

val show_attr : attr -> string

val show_tbl : tbl -> string

val show_const : const -> string

val show_pred : pred -> string

val show : sql_expr -> string
