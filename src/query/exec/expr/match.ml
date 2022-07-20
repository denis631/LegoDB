open Core
open Storage
open Utils

(* TODO: call the Module Expr, intead of Match, as the same tree will be used for other purposes *)
(* TODO: refactor the expression type, should be able to evaluate to any primitive type *)
module Expr = struct
  type boolean = And of boolean list | Or of boolean list | Eq of t * t
  [@@deriving show { with_path = false }]

  and leaf = Const of Value.t | TableAttr of Schema.Iu.t [@@deriving show]
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
end
