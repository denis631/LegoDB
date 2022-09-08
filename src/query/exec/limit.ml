open Common
open Core

type limit = { child_op : op; limit : int; mutable current_idx : int }
type op += Limit of limit

let make ~child_op ~limit = Limit { child_op; limit; current_idx = 0 }
let has_iu root_has_iu iu limit = root_has_iu iu limit.child_op
let open_op fs ctx limit = fs.open_op ctx limit.child_op
let close_op fs ctx limit = fs.close_op ctx limit.child_op

let next fs ctx limit =
  match fs.next ctx limit.child_op with
  | Some record ->
      if limit.current_idx < limit.limit then (
        limit.current_idx <- limit.current_idx + 1;
        Some record)
      else None
  | None -> None
