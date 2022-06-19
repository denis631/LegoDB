open Utils

type tbl_name = string [@@deriving show]
type col_name = string [@@deriving show]
type path = string [@@deriving show]
type attr = AttrName of string | Star [@@deriving show]
type const = Int of int | Str of string [@@deriving show]
type pred = EqConst of attr * const | EqAttr of attr * attr [@@deriving show]
type key_type = PrimaryKey [@@deriving show]

type tbl_elt =
  | ColDef of string * Value_type.t
  | ConstraintDef of key_type * col_name list
[@@deriving show]

type sql_expr =
  (* DDL *)
  | CreateTbl of tbl_name * tbl_elt list
  | DropTbl of tbl_name list
  (* DML *)
  | Select of attr list * tbl_name list * pred list option
  | Copy of tbl_name * path
[@@deriving show]
