open Common
open Core
open Storage
open Utils

type projection = {
  schema : Schema.t;
  child_op : op;
  input_schema : Schema.t;
  buffer : RecordBuffer.t;
}

type op += Projection of projection

let make fs ~schema ~child_op =
  Projection
    {
      schema;
      child_op;
      input_schema = fs.output_schema child_op;
      buffer = RecordBuffer.make @@ RecordBuffer.length_from_schema schema;
    }

let has_iu root_has_iu iu projection = root_has_iu iu projection.child_op
let open_op fs ctx projection = fs.open_op ctx projection.child_op
let close_op fs ctx projection = fs.close_op ctx projection.child_op

let next fs ctx projection =
  let open Option in
  let project record =
    Record.Data.copy_to (snd record) projection.input_schema projection.buffer
      projection.schema;
    (fst record, projection.buffer)
  in
  fs.next ctx projection.child_op >>| project
