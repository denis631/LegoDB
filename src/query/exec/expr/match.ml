open Storage
open BatteriesExceptionless

module Expr = struct
  type bool =
    | And of bool list
    | Or of bool list
    | Eq of t * t

  and leaf =
    | Const of Value.t
    (* NOTE: logical *)
    | TableAttr of Table.Iu.t
    (* NOTE: physical *)
    | TableAttrIdx of int

  and t =
    | BoolExpr of bool
    | Leaf of leaf

  let rec prepare iu_idx_map expr =
    let prepare_leaf = function
      | Const x ->
          Const x
      | TableAttr iu ->
          TableAttrIdx (Option.get @@ Hashtbl.find iu_idx_map iu)
      | TableAttrIdx _ ->
          failwith "illegal node in the tree"
    in
    let rec prepare_bool = function
      | And expr_lst ->
          And (List.map prepare_bool expr_lst)
      | Or expr_lst ->
          Or (List.map prepare_bool expr_lst)
      | Eq (e1, e2) ->
          Eq (prepare iu_idx_map e1, prepare iu_idx_map e2)
    in
    match expr with
    | Leaf leaf ->
        Leaf (prepare_leaf leaf)
    | BoolExpr bool ->
        BoolExpr (prepare_bool bool)


  let rec ius expr =
    let rec ius_of_bool = function
      | And expr_lst ->
          List.unique @@ List.concat_map ius_of_bool expr_lst
      | Or expr_lst ->
          List.unique @@ List.concat_map ius_of_bool expr_lst
      | Eq (e1, e2) ->
          ius e1 @ ius e2
    in
    let ius_of_leaf = function
      | Const _ ->
          []
      | TableAttr iu ->
          [ iu ]
      | TableAttrIdx _ ->
          failwith "TODO: invalid node"
    in
    match expr with
    | Leaf leaf ->
        ius_of_leaf leaf
    | BoolExpr bool ->
        ius_of_bool bool


  let rec eval tuple expr =
    let rec eval_bool = function
      | And expr_lst ->
          List.fold_left (fun acc expr -> acc && eval_bool expr) true expr_lst
      | Or expr_lst ->
          List.fold_left (fun acc expr -> acc || eval_bool expr) true expr_lst
      | Eq (e1, e2) ->
          let lhs = eval tuple e1 in
          let rhs = eval tuple e2 in
          Value.eq lhs rhs
    in
    let eval_leaf = function
      | Const x ->
          x
      | TableAttr _ ->
          failwith "TODO: implement iu -> idx mapping"
      | TableAttrIdx idx ->
          Storage.Tuple.get tuple idx
    in
    match expr with
    | Leaf leaf ->
        eval_leaf leaf
    | BoolExpr bool ->
        Value.Bool (eval_bool bool)
end
