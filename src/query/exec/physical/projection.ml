open BatteriesExceptionless
open Storage
open Common

type projection =
  { attributes : Table.RegularTbl.Iu.t list
  ; child_op : op
  }

type op += Projection of projection

let make ~attributes ~child_op = Projection { attributes; child_op }

let has_iu root_has_iu iu projection = root_has_iu iu projection.child_op

let prepare _ projection = projection

let next root_next ctx projection =
  match root_next ctx projection.child_op with
  | Some (tuple, schema) ->
      let unzip coll =
        let xs = List.map fst coll in
        let ys = List.map snd coll in
        (xs, ys)
      in
      let new_tuple, new_schema =
        let get_val iu =
          let idx = Option.get @@ List.index_of iu schema in
          List.nth tuple idx
        in
        List.map (fun iu -> (get_val iu, iu)) projection.attributes |> unzip
      in
      Some (new_tuple, new_schema)
  | None ->
      None
