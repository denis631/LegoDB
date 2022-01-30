open Storage

let rec predicate_pushdown iu selection =
  let predicate, childOp =
    match selection with
    | Physical.Selection.Selection t ->
        (t.predicate, t.childOp)
    | _ ->
        failwith "can not happen"
  in
  let make_selection = Physical.Selection.make ~predicate in
  match childOp with
  | Physical.Hash_join.HashJoin join ->
    ( match
        ( Physical.Operators.has_iu iu join.leftOp
        , Physical.Operators.has_iu iu join.rightOp )
      with
    | true, true ->
        Physical.Hash_join.make
          ~leftOp:(predicate_pushdown iu @@ make_selection ~childOp:join.leftOp)
          ~rightOp:
            (predicate_pushdown iu @@ make_selection ~childOp:join.rightOp)
          ~hash_key_ius:join.hash_key_ius
    | true, false ->
        Physical.Hash_join.make
          ~leftOp:(predicate_pushdown iu @@ make_selection ~childOp:join.leftOp)
          ~rightOp:join.rightOp
          ~hash_key_ius:join.hash_key_ius
    | false, true ->
        Physical.Hash_join.make
          ~leftOp:join.leftOp
          ~rightOp:
            (predicate_pushdown iu @@ make_selection ~childOp:join.rightOp)
          ~hash_key_ius:join.hash_key_ius
    | _ ->
        selection )
  | _ ->
      selection


let rec optimize = function
  | Logical.Operators.TableScan tbl ->
      Physical.Table_scan.make ~iter:(Table.Iter.make tbl) ~ius:(Table.ius tbl)
  | Logical.Operators.Selection
      ( Expr.Match.Expr.Eq
          ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs)
          , Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) )
      , Logical.Operators.CrossProduct (leftOp, rightOp) ) ->
      let opt_leftOp = optimize leftOp in
      let opt_rightOp = optimize rightOp in
      let key_ius =
        if Physical.Operators.has_iu lhs opt_leftOp
        then ([ lhs ], [ rhs ])
        else ([ rhs ], [ lhs ])
      in
      Physical.Hash_join.make
        ~leftOp:opt_leftOp
        ~rightOp:opt_rightOp
        ~hash_key_ius:key_ius
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
      | ( Expr.Match.Expr.Eq
            ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr iu)
            , Expr.Match.Expr.Leaf (Expr.Match.Expr.Const _) )
        , _ ) ->
          predicate_pushdown
            iu
            (Physical.Selection.make ~predicate:match_tree ~childOp)
      | _ ->
          Physical.Selection.make ~predicate:match_tree ~childOp )
  | Logical.Operators.Projection (proj_attrs, op) ->
      Physical.Projection.make ~attributes:proj_attrs ~childOp:(optimize op)
  | Logical.Operators.CrossProduct (_, _) ->
      failwith
        "TODO: no real cross products are allowed to be produced, only joins"
