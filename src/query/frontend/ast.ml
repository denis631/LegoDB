(* TODO: support aliasing *)
type attr = AttrName of String.t

(* TODO: support aliasing *)
type tbl = TblName of String.t

type const =
  | Int of int
  | Str of String.t

type predicate =
  | EqConst of attr * const
  | EqAttr of attr * attr

type select_expr =
  { attr_lst : attr list
  ; tbl_lst : tbl list
  ; pred_lst : predicate list
  }

type sql_expr = Select of select_expr

let show_attr = function AttrName x -> x

let show_tbl = function TblName x -> x

let show_const = function Int x -> string_of_int x | Str s -> s

let show_pred = function
  | EqConst (x, y) ->
      show_attr x ^ "=" ^ show_const y
  | EqAttr (x, y) ->
      show_attr x ^ "=" ^ show_attr y


let show = function
  | Select stmt ->
      let attrs = String.concat ", " @@ List.map show_attr stmt.attr_lst in
      let tbls = String.concat ", " @@ List.map show_tbl stmt.tbl_lst in
      let preds = String.concat ", " @@ List.map show_pred stmt.pred_lst in
      "attrs: " ^ attrs ^ "\n" ^ "tbls: " ^ tbls ^ "\n" ^ "preds: " ^ preds
