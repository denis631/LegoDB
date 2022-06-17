open Storage

module Expr : sig
  type bool = And of bool list | Or of bool list | Eq of t * t
  and leaf = Const of Value.t | TableAttr of Table.T.Iu.t
  and t = BoolExpr of bool | Leaf of leaf

  val ius : t -> Table.T.Iu.t list
  val eval : Tuple.t * Table.T.Iu.t list -> t -> Value.t
  val show : t -> string
end
