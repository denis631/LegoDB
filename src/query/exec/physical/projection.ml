open BatteriesExceptionless
open Storage
open Common

type projection =
  { attributes : Table.Iu.t list
  ; childOp : op
  }

type op += Projection of projection

let make ~attributes ~childOp = Projection { attributes; childOp }

let has_iu root_has_iu iu projection = root_has_iu iu projection.childOp

let prepare _ projection = projection

let next root_next ctx projection =
  match root_next ctx projection.childOp with
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
