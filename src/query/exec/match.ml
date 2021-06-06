open Storage
open BatteriesExceptionless

type tpl_idx = int

type bool_expr =
  | And of bool_expr list
  | Or of bool_expr list
  | Eq of expr * expr

and leaf =
  | Const of Value.t
  (* NOTE: logical *)
  | TableAttr of Table.iu
  (* NOTE: physical *)
  | TableAttrIdx of tpl_idx

and expr = Leaf of leaf

let rec eval_bool_expr tuple = function
  | And expr_lst ->
      List.fold_left
        (fun acc expr -> acc && eval_bool_expr tuple expr)
        true
        expr_lst
  | Or expr_lst ->
      List.fold_left
        (fun acc expr -> acc || eval_bool_expr tuple expr)
        true
        expr_lst
  | Eq (e1, e2) ->
      let lhs = eval_expr tuple e1 in
      let rhs = eval_expr tuple e2 in
      Value.eq lhs rhs


and eval_leaf tuple = function
  | Const x ->
      x
  | TableAttr _ ->
      failwith "TODO: implement iu -> idx mapping"
  | TableAttrIdx idx ->
      Storage.Tuple.get tuple idx


and eval_expr tuple = function Leaf leaf -> eval_leaf tuple leaf

let rec ius_of_bool_expr = function
  | And expr_lst ->
      List.unique @@ List.concat_map ius_of_bool_expr expr_lst
  | Or expr_lst ->
      List.unique @@ List.concat_map ius_of_bool_expr expr_lst
  | Eq (e1, e2) ->
      ius_of_expr e1 @ ius_of_expr e2


and ius_of_leaf = function
  | Const _ ->
      []
  | TableAttr iu ->
      [ iu ]
  | TableAttrIdx _ ->
      failwith "TODO: invalid node"


and ius_of_expr = function Leaf leaf -> ius_of_leaf leaf

let rec prepare_bool_expr iu_idx_map = function
  | And expr_lst ->
      And (List.map (prepare_bool_expr iu_idx_map) expr_lst)
  | Or expr_lst ->
      Or (List.map (prepare_bool_expr iu_idx_map) expr_lst)
  | Eq (e1, e2) ->
      Eq (prepare_expr iu_idx_map e1, prepare_expr iu_idx_map e2)


and prepare_leaf iu_idx_map = function
  | Const x ->
      Const x
  | TableAttr iu ->
      TableAttrIdx (Option.get @@ Hashtbl.find iu_idx_map iu)
  | TableAttrIdx _ ->
      failwith "illegal node in the tree"


and prepare_expr iu_idx_map = function
  | Leaf leaf ->
      Leaf (prepare_leaf iu_idx_map leaf)
