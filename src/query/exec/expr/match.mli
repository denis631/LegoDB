open Storage

module Expr : sig
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

  val prepare : (Table.Iu.t, int) Hashtbl.t -> t -> t

  val ius : t -> Table.Iu.t list

  val eval : Tuple.t -> t -> Value.t
end
