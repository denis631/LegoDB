open Expr
open Utils

type tbl_name = string [@@deriving show]
type col_name = string [@@deriving show]
type path = string [@@deriving show]
type attr_name = string [@@deriving show]
type attr = Attr of attr_name | Star [@@deriving show]
type key_type = PrimaryKey [@@deriving show]
type from_clause = FromClause of tbl_name list [@@deriving show]
type where_clause = WhereClause of Match.Expr.boolean [@@deriving show]
type order_expr = OrderExpr of attr_name * Order.direction [@@deriving show]
type order_clause = OrderClause of order_expr list [@@deriving show]

type tbl_elt =
  | ColDef of string * Value_type.t
  | ConstraintDef of key_type * col_name list
[@@deriving show]

type sql_expr =
  (* DDL *)
  | CreateTbl of tbl_name * tbl_elt list
  | DropTbl of tbl_name list
  (* DML *)
  | Select of
      attr list * from_clause * where_clause option * order_clause option
  | Copy of tbl_name * path
[@@deriving show]
