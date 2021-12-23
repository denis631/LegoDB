open Storage
open BatteriesExceptionless

module Expr = struct
  type bool =
    | And of bool list
    | Or of bool list
    | Eq of t * t

  and leaf =
    | Const of Value.t
    | TableAttr of Table.Iu.t

  and t =
    | BoolExpr of bool
    | Leaf of leaf

  let rec ius expr =
    let rec ius_of_bool = function
      | And expr_lst ->
          List.unique @@ List.concat_map ius_of_bool expr_lst
      | Or expr_lst ->
          List.unique @@ List.concat_map ius_of_bool expr_lst
      | Eq (e1, e2) ->
          ius e1 @ ius e2
    in
    let ius_of_leaf = function Const _ -> [] | TableAttr iu -> [ iu ] in
    match expr with
    | Leaf leaf ->
        ius_of_leaf leaf
    | BoolExpr bool ->
        ius_of_bool bool


  let rec eval (tuple, schema) expr =
    let rec eval_bool = function
      | And expr_lst ->
          List.fold_left (fun acc expr -> acc && eval_bool expr) true expr_lst
      | Or expr_lst ->
          List.fold_left (fun acc expr -> acc || eval_bool expr) true expr_lst
      | Eq (e1, e2) ->
          let lhs = eval (tuple, schema) e1 in
          let rhs = eval (tuple, schema) e2 in
          Value.eq lhs rhs
    in
    let eval_leaf = function
      | Const x ->
          x
      | TableAttr iu ->
          List.map2 (fun x y -> (x, y)) tuple schema
          |> List.find (Table.Iu.eq iu % snd)
          |> Option.get
          |> fst
    in
    match expr with
    | Leaf leaf ->
        eval_leaf leaf
    | BoolExpr bool ->
        Value.Bool (eval_bool bool)


  let rec show expr =
    let rec show_bool = function
      | And expr_lst ->
          expr_lst |> List.map show_bool |> String.join " AND "
      | Or expr_lst ->
          expr_lst |> List.map show_bool |> String.join " OR "
      | Eq (e1, e2) ->
          show e1 ^ " = " ^ show e2
    in
    let show_leaf = function
      | Const x ->
          Value.show x
      | TableAttr iu ->
          Table.Iu.show iu
    in
    match expr with
    | Leaf leaf ->
        show_leaf leaf
    | BoolExpr bool ->
        show_bool bool
end
