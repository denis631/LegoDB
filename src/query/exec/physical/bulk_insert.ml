open Common
open Core
open Storage

type bulk_inserter = {
  child_op : op;
  meta : Storage.Table.Meta.t;
  mutable seq : Storage.Tuple.t Sequence.t option;
}

type op += BulkInserter of bulk_inserter

let make ~child_op ~meta = BulkInserter { child_op; meta; seq = None }

let open_op fs inserter =
  fs.open_op inserter.child_op;
  let seq =
    let f () =
      Option.map ~f:(fun t -> (Tuple_buffer.clone t, ())) @@ fs.next () inserter.child_op
    in
    Sequence.unfold ~init:() ~f
  in
  inserter.seq <- Some seq

let close_op fs inserter =
  fs.close_op inserter.child_op;
  inserter.seq <- None

(* TODO: how about performing bulk loading in batches of 1000 tuples *)
let next _ _ inserter =
  let insert =
    Table.Crud.Record.bulk_insert
      (Database.db_session_ref Database.instance)
      inserter.meta
  in
  Option.iter ~f:insert inserter.seq;
  None
