open Storage

module Expr : sig
  type bool = And of bool list | Or of bool list | Eq of t * t
  and leaf = Const of Value.t | TableAttr of Table.RegularTbl.Iu.t
  and t = BoolExpr of bool | Leaf of leaf

  val ius : t -> Table.RegularTbl.Iu.t list
  val eval : Tuple.t * Table.RegularTbl.Iu.t list -> t -> Value.t
  val show : t -> string
end
