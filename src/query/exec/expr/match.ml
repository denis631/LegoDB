open Core
open Utils

(* TODO: call the Module Expr, intead of Match, as the same tree will be used for other purposes *)
(* TODO: refactor the expression type, should be able to evaluate to any primitive type *)
module Expr = struct
  type boolean = And of boolean list | Or of boolean list | Eq of t * t
  [@@deriving show { with_path = false }]

  and leaf =
    | Const of Value.t
    | TableAttrName of string
    | TableAttr of Schema.Iu.t
  [@@deriving show]

  and t = Bool of boolean | Leaf of leaf [@@deriving show]

  let rec ius expr =
    let rec ius_of_bool = function
      | And expr_lst -> List.concat_map ~f:ius_of_bool expr_lst
      | Or expr_lst -> List.concat_map ~f:ius_of_bool expr_lst
      | Eq (e1, e2) -> ius e1 @ ius e2
    in
    let ius_of_leaf = function TableAttr iu -> [ iu ] | _ -> [] in
    match expr with
    | Leaf leaf -> ius_of_leaf leaf
    | Bool bool -> ius_of_bool bool
end
