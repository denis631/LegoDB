type attr =
  | AttrName of string
  | Star

type tbl = TblName of string

type const =
  | Int of int
  | Str of string

type pred =
  | EqConst of attr * const
  | EqAttr of attr * attr

type sql_expr = Select of attr list * tbl list * pred list option

let show_attr = function AttrName x -> x | Star -> "*"

let show_tbl = function TblName x -> x

let show_const = function Int x -> string_of_int x | Str s -> s

let show_pred = function
  | EqConst (x, y) ->
      show_attr x ^ "=" ^ show_const y
  | EqAttr (x, y) ->
      show_attr x ^ "=" ^ show_attr y


let show = function
  | Select (attr_lst, tbl_lst, pred_lst) ->
      let attrs = String.concat ", " @@ List.map show_attr attr_lst in
      let tbls = String.concat ", " @@ List.map show_tbl tbl_lst in
      let preds = if Option.is_some pred_lst then Option.get pred_lst else [] in
      let preds = String.concat ", " @@ List.map show_pred preds in
      "attrs: " ^ attrs ^ "\n" ^ "tbls: " ^ tbls ^ "\n" ^ "preds: " ^ preds
