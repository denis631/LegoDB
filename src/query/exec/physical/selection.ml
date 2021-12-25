open BatteriesExceptionless
open Storage
open Expr
open Common

type selection =
  { predicate : Match.Expr.bool
  ; childOp : op
  }

type op += Selection of selection

let make ~predicate ~childOp = Selection { predicate; childOp }

let has_iu root_has_iu iu selection = root_has_iu iu selection.childOp

let prepare _ selection = selection

let next root_next ctx selection =
  let rec probe () =
    match root_next ctx selection.childOp with
    | Some x ->
        let is_true = Value.eq @@ Value.Bool true in
        if is_true
           @@ Match.Expr.eval x
           @@ Match.Expr.BoolExpr selection.predicate
        then Some x
        else probe ()
    | None ->
        None
  in
  probe ()
