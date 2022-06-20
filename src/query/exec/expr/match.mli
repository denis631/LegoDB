open Storage

module Expr : sig
  type boolean = And of boolean list | Or of boolean list | Eq of t * t
  [@@deriving show]

  and leaf = Const of Value.t | TableAttr of Table.T.Iu.t [@@deriving show]
  and t = Bool of boolean | Leaf of leaf [@@deriving show]

  val ius : t -> Table.T.Iu.t list
  val eval : Tuple.t * Table.T.Iu.t list -> t -> Value.t
end
