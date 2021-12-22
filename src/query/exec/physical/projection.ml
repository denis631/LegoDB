open BatteriesExceptionless
open Storage
open Common

type projection =
  { attributes : Table.Iu.t list
  ; childOp : op
  }

type op += Projection of projection

let make ~attributes ~childOp = Projection { attributes; childOp }

let prepare _ projection = projection

let next root_next ctx projection =
  match root_next ctx projection.childOp with
  | Some (tuple, schema) ->
      let should_project_iu iu =
        List.exists (Table.Iu.eq iu) projection.attributes
      in
      let zip = List.map2 (fun x y -> (x, y)) in
      let unzip coll =
        let xs = List.map fst coll in
        let ys = List.map snd coll in
        (xs, ys)
      in
      let new_tuple, new_schema =
        zip tuple schema |> List.filter (should_project_iu % snd) |> unzip
      in
      Some (new_tuple, new_schema)
  | None ->
      None
