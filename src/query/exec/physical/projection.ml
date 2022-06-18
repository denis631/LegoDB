open Storage
open Common
open Core

type projection = { attributes : Table.T.Iu.t list; child_op : op }
type op += Projection of projection

let make ~attributes ~child_op = Projection { attributes; child_op }
let has_iu root_has_iu iu projection = root_has_iu iu projection.child_op
let open_op f projection = f projection.child_op
let close_op f projection = f projection.child_op

let next root_next ctx projection =
  match root_next ctx projection.child_op with
  | Some (tuple, schema) ->
      let unzip coll =
        List.fold_right coll
          ~f:(fun (x, y) (xs, ys) -> (x :: xs, y :: ys))
          ~init:([], [])
      in
      let new_tuple, new_schema =
        let get_val iu =
          let idx =
            fst @@ List.findi_exn ~f:(fun _ -> Table.T.Iu.eq iu) schema
          in
          List.nth_exn tuple idx
        in
        List.map ~f:(fun iu -> (get_val iu, iu)) projection.attributes |> unzip
      in
      Some (new_tuple, new_schema)
  | None -> None
