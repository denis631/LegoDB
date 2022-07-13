open Common
open Core
open Storage

type projection = {
  schema : Schema.t;
  child_op : op;
  input_schema : Storage.Schema.t;
  buffer : Storage.Tuple_buffer.t;
}

type op += Projection of projection

let make fs ~schema ~child_op =
  Projection
    {
      schema;
      child_op;
      input_schema = fs.output_schema child_op;
      buffer = Tuple_buffer.make @@ Tuple_buffer.length_from_schema schema;
    }

let has_iu root_has_iu iu projection = root_has_iu iu projection.child_op
let open_op fs projection = fs.open_op projection.child_op
let close_op fs projection = fs.close_op projection.child_op

let next fs ctx projection =
  let open Option in
  let project tuple =
    Storage.Tuple.copy_tuple tuple projection.input_schema projection.buffer
      projection.schema;
    projection.buffer
  in
  fs.next ctx projection.child_op >>| project
