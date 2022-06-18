open Storage
open Expr
open Common

type selection = { predicate : Match.Expr.bool; child_op : op }
type op += Selection of selection

let make ~predicate ~child_op = Selection { predicate; child_op }
let has_iu root_has_iu iu selection = root_has_iu iu selection.child_op
let open_op fs selection = fs.open_op selection.child_op
let close_op fs selection = fs.close_op selection.child_op

let next fs ctx selection =
  let rec probe () =
    match fs.next ctx selection.child_op with
    | Some x ->
        let is_true = Value.equal (Value.Bool true) in
        if is_true @@ Match.Expr.eval x @@ Match.Expr.Bool selection.predicate
        then Some x
        else probe ()
    | None -> None
  in
  probe ()
