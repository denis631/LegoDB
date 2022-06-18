open Core
open Storage
open Utils

module Expr = struct
  type bool = And of bool list | Or of bool list | Eq of t * t
  and leaf = Const of Value.t | TableAttr of Table.T.Iu.t
  and t = Bool of bool | Leaf of leaf

  let rec ius expr =
    let rec ius_of_bool = function
      | And expr_lst -> List.concat_map ~f:ius_of_bool expr_lst
      | Or expr_lst -> List.concat_map ~f:ius_of_bool expr_lst
      | Eq (e1, e2) -> ius e1 @ ius e2
    in
    let ius_of_leaf = function Const _ -> [] | TableAttr iu -> [ iu ] in
    match expr with
    | Leaf leaf -> ius_of_leaf leaf
    | Bool bool -> ius_of_bool bool

  let rec eval (tuple, schema) expr =
    let rec eval_bool = function
      | And expr_lst ->
          List.fold_left
            ~f:(fun acc expr -> acc && eval_bool expr)
            ~init:true expr_lst
      | Or expr_lst ->
          List.fold_left
            ~f:(fun acc expr -> acc || eval_bool expr)
            ~init:true expr_lst
      | Eq (e1, e2) ->
          let lhs = eval (tuple, schema) e1 in
          let rhs = eval (tuple, schema) e2 in
          Value.equal lhs rhs
    in
    let eval_leaf = function
      | Const x -> x
      | TableAttr iu ->
          List.zip_exn tuple schema
          |> List.find ~f:(snd %> Table.T.Iu.eq iu)
          |> Stdlib.Option.get |> fst
    in
    match expr with
    | Leaf leaf -> eval_leaf leaf
    | Bool bool -> Value.Bool (eval_bool bool)

  let rec show expr =
    let rec show_bool = function
      | And expr_lst ->
          expr_lst |> List.map ~f:show_bool |> String.concat ~sep:" AND "
      | Or expr_lst ->
          expr_lst |> List.map ~f:show_bool |> String.concat ~sep:" OR "
      | Eq (e1, e2) -> show e1 ^ " = " ^ show e2
    in
    let show_leaf = function
      | Const x -> Value.show x
      | TableAttr iu -> Table.T.Iu.show iu
    in
    match expr with Leaf leaf -> show_leaf leaf | Bool bool -> show_bool bool
end
