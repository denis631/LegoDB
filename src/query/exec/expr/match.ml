open Core
open Storage
open Utils

module Expr = struct
  type boolean = And of boolean list | Or of boolean list | Eq of t * t
  [@@deriving show]

  and leaf = Const of Value.t | TableAttr of Table.T.Iu.t [@@deriving show]
  and t = Bool of boolean | Leaf of leaf [@@deriving show]

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
          Sequence.of_list expr_lst |> Sequence.map ~f:eval_bool
          |> Sequence.fold ~init:true ~f:( && )
      | Or expr_lst ->
          Sequence.of_list expr_lst |> Sequence.map ~f:eval_bool
          |> Sequence.fold ~init:true ~f:( || )
      | Eq (e1, e2) ->
          let lhs = eval (tuple, schema) e1 in
          let rhs = eval (tuple, schema) e2 in
          Value.equal lhs rhs
    in
    let eval_leaf = function
      | Const x -> x
      | TableAttr iu ->
          List.zip_exn tuple schema
          |> List.find_exn ~f:(snd %> Table.T.Iu.eq iu)
          |> fst
    in
    match expr with
    | Leaf leaf -> eval_leaf leaf
    | Bool bool -> Value.Bool (eval_bool bool)
end
