open Storage

let rec predicate_pushdown iu selection =
  let predicate, child_op =
    match selection with
    | Physical.Selection.Selection t -> (t.predicate, t.child_op)
    | _ -> failwith "can not happen"
  in
  let make_selection = Physical.Selection.make ~predicate in
  match child_op with
  | Physical.Hash_join.HashJoin join -> (
      match
        ( Physical.Operators.has_iu iu join.left_op,
          Physical.Operators.has_iu iu join.right_op )
      with
      | true, true ->
          Physical.Hash_join.make
            ~left_op:
              (predicate_pushdown iu @@ make_selection ~child_op:join.left_op)
            ~right_op:
              (predicate_pushdown iu @@ make_selection ~child_op:join.right_op)
            ~hash_key_ius:join.hash_key_ius
      | true, false ->
          Physical.Hash_join.make
            ~left_op:
              (predicate_pushdown iu @@ make_selection ~child_op:join.left_op)
            ~right_op:join.right_op ~hash_key_ius:join.hash_key_ius
      | false, true ->
          Physical.Hash_join.make ~left_op:join.left_op
            ~right_op:
              (predicate_pushdown iu @@ make_selection ~child_op:join.right_op)
            ~hash_key_ius:join.hash_key_ius
      | _ -> selection)
  | _ -> selection

let rec optimize db = function
  | Logical.Operators.TableScan tbl ->
      Physical.Table_scan.make
        ~iter:(Table.Iter.make (Database.db_session_ref db) tbl)
        ~ius:(Table.ius tbl)
  | Logical.Operators.Selection
      ( Expr.Match.Expr.Eq
          ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs),
            Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ),
        Logical.Operators.CrossProduct (left_op, right_op) ) ->
      let opt_left_op = optimize db left_op in
      let opt_right_op = optimize db right_op in
      let key_ius =
        if Physical.Operators.has_iu lhs opt_left_op then ([ lhs ], [ rhs ])
        else ([ rhs ], [ lhs ])
      in
      Physical.Hash_join.make ~left_op:opt_left_op ~right_op:opt_right_op
        ~hash_key_ius:key_ius
  | Logical.Operators.Selection (match_tree, op) -> (
      let child_op = optimize db op in
      match (match_tree, child_op) with
      | ( Expr.Match.Expr.Eq
            ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr lhs),
              Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr rhs) ),
          Physical.Hash_join.HashJoin join ) ->
          Physical.Hash_join.make ~left_op:join.left_op ~right_op:join.right_op
            ~hash_key_ius:
              (lhs :: fst join.hash_key_ius, rhs :: snd join.hash_key_ius)
      | ( Expr.Match.Expr.Eq
            ( Expr.Match.Expr.Leaf (Expr.Match.Expr.TableAttr iu),
              Expr.Match.Expr.Leaf (Expr.Match.Expr.Const _) ),
          _ ) ->
          predicate_pushdown iu
            (Physical.Selection.make ~predicate:match_tree ~child_op)
      | _ -> Physical.Selection.make ~predicate:match_tree ~child_op)
  | Logical.Operators.Projection (proj_attrs, op) ->
      Physical.Projection.make ~attributes:proj_attrs ~child_op:(optimize db op)
  | Logical.Operators.CrossProduct (_, _) ->
      failwith
        "TODO: no real cross products are allowed to be produced, only joins"
