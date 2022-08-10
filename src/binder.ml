open Core
open Expr
open Utils
module TableMeta = Table_meta

module Ast = struct
  include Frontend.Ast

  type attr = Attr of Schema.Iu.t | Star [@@deriving show]

  type order_expr = OrderExpr of Schema.Iu.t * Order.direction
  [@@deriving show]

  type order_clause = OrderClause of order_expr list [@@deriving show]

  type sql_expr =
    (* DDL *)
    | CreateTbl of tbl_name * tbl_elt list
    | DropTbl of tbl_name list
    (* DML *)
    | Select of
        attr list * from_clause * where_clause option * order_clause option
    | Copy of tbl_name * path
  [@@deriving show]
end

type ctx = { catalog : Catalog.t; mutable tbls : TableMeta.t list }

let make_ctx catalog () = { catalog; tbls = [] }

let bind_tbl ctx tbl_name =
  ctx.tbls <- Catalog.find_tbl ctx.catalog tbl_name :: ctx.tbls;
  tbl_name

let bind_attr ctx attr =
  let find_attr_for_tbl (tbl_meta : TableMeta.t) =
    let filter_col (iu : Schema.Iu.t) = String.equal iu.column attr in
    List.find ~f:filter_col tbl_meta.schema
  in
  match List.filter_map ~f:find_attr_for_tbl ctx.tbls with
  | [ x ] -> x
  | _ :: _ -> failwith @@ "There are multiple tables with the attribute " ^ attr
  | [] -> failwith @@ "Attribute with name " ^ attr ^ " not found"

let rec bind_match_expr ctx expr =
  let bind_leaf = function
    | Expr.Match.Expr.TableAttrName attr_name ->
        Expr.Match.Expr.TableAttr (bind_attr ctx attr_name)
    | x -> x
  in
  match expr with
  | Match.Expr.And list ->
      Match.Expr.And (List.map ~f:(bind_match_expr ctx) list)
  | Match.Expr.Or list -> Match.Expr.Or (List.map ~f:(bind_match_expr ctx) list)
  | Match.Expr.Eq (a, b) ->
      let bind = function
        | Match.Expr.Bool b -> Match.Expr.Bool (bind_match_expr ctx b)
        | Match.Expr.Leaf l -> Match.Expr.Leaf (bind_leaf l)
      in
      Match.Expr.Eq (bind a, bind b)

let bind_order_expr ctx (Frontend.Ast.OrderExpr (name, dir)) =
  Ast.OrderExpr (bind_attr ctx name, dir)

let bind catalog ast =
  match ast with
  | Frontend.Ast.Select
      (attr_list, FromClause tbl_list, where_clause_opt, order_clause_opt) ->
      let ctx = make_ctx catalog () in
      (* Populates the context with the tables *)
      let _ = List.map ~f:(bind_tbl ctx) tbl_list in

      (* Bind attribute names to IUs of the accessed tables *)
      let attrs =
        List.map
          ~f:(function
            | Frontend.Ast.Attr attr -> Ast.Attr (bind_attr ctx attr)
            | Frontend.Ast.Star -> Ast.Star)
          attr_list
      in
      let where_clause =
        match where_clause_opt with
        | None -> None
        | Some (WhereClause expr) ->
            Some (Ast.WhereClause (bind_match_expr ctx expr))
      in
      let order_clause =
        match order_clause_opt with
        | None -> None
        | Some (OrderClause order_expr_list) ->
            Some
              (Ast.OrderClause
                 (List.map ~f:(bind_order_expr ctx) order_expr_list))
      in
      Ast.Select (attrs, FromClause tbl_list, where_clause, order_clause)
  | Frontend.Ast.CreateTbl (tbl_name, tbl_elt_list) ->
      Ast.CreateTbl (tbl_name, tbl_elt_list)
  | Frontend.Ast.DropTbl tbl_name_list -> Ast.DropTbl tbl_name_list
  | Frontend.Ast.Copy (tbl_name, path) -> Ast.Copy (tbl_name, path)
