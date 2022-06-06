open Utils

type tbl_name = string
type col_name = string
type path = string
type attr = AttrName of string | Star
type const = Int of int | Str of string
type pred = EqConst of attr * const | EqAttr of attr * attr
type key_type = PrimaryKey

type tbl_elt =
  | ColDef of string * Value_type.t
  | ConstraintDef of key_type * col_name list

type ddl_expr = CreateTbl of tbl_name * tbl_elt list

type dml_expr =
  | Select of attr list * tbl_name list * pred list option
  | Copy of tbl_name * path

type sql_expr = DML of dml_expr | DDL of ddl_expr

let show_attr = function AttrName x -> x | Star -> "*"
let show_const = function Int x -> string_of_int x | Str s -> s

let show_pred = function
  | EqConst (x, y) -> show_attr x ^ "=" ^ show_const y
  | EqAttr (x, y) -> show_attr x ^ "=" ^ show_attr y

let show = function
  | DML (Select (attr_lst, tbl_lst, pred_lst)) ->
      let attrs = String.concat ", " @@ List.map show_attr attr_lst in
      let tbls = String.concat ", " tbl_lst in
      let preds = if Option.is_some pred_lst then Option.get pred_lst else [] in
      let preds = String.concat ", " @@ List.map show_pred preds in
      "attrs: " ^ attrs ^ "\n" ^ "tbls: " ^ tbls ^ "\n" ^ "preds: " ^ preds
  | _ -> ""
