open Storage

let rec optimize = function
  | Logical.Operators.TableScan tbl ->
      Physical.Table_scan.make ~iter:(Table.Iter.make tbl) ~ius:(Table.ius tbl)
  | Logical.Operators.Selection
      ( Expr.Match.Expr.Eq
          ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs)
          , Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) )
      , Logical.Operators.CrossProduct (leftOp, rightOp) ) ->
      Physical.Hash_join.make
        ~leftOp:(optimize leftOp)
        ~rightOp:(optimize rightOp)
        ~hash_key_ius:([ lhs ], [ rhs ])
  | Logical.Operators.Selection (match_tree, op) ->
      let childOp = optimize op in
      ( match (match_tree, childOp) with
      | ( Expr.Match.Expr.Eq
            ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs)
            , Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) )
        , Physical.Hash_join.HashJoin join ) ->
          Physical.Hash_join.make
            ~leftOp:join.leftOp
            ~rightOp:join.rightOp
            ~hash_key_ius:
              (lhs :: fst join.hash_key_ius, rhs :: snd join.hash_key_ius)
      | _ ->
          Physical.Selection.make ~predicate:match_tree ~childOp )
  | Logical.Operators.Projection (proj_attrs, op) ->
      Physical.Projection.make ~attributes:proj_attrs ~childOp:(optimize op)
  | Logical.Operators.CrossProduct (_, _) ->
      failwith
        "TODO: no real cross products are allowed to be produced, only joins"
