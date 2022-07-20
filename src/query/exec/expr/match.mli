open Storage
open Utils

module Expr : sig
  type boolean = And of boolean list | Or of boolean list | Eq of t * t
  [@@deriving show]

  and leaf = Const of Value.t | TableAttr of Schema.Iu.t [@@deriving show]
  and t = Bool of boolean | Leaf of leaf [@@deriving show]

  val ius : t -> Schema.t
end
