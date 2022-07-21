open Common
open Core
open Storage
open Utils
module Cursor = Database.Session.Cursor

module Result = struct
  include Result

  let ( let* ) r f = Result.bind r ~f
end

type bulk_inserter = { child_op : op; meta : TableMeta.t; cursor : Cursor.t }
type op += BulkInserter of bulk_inserter

let make ~child_op ~(meta : TableMeta.t) =
  (* TODO: not sure the session should be tied by catalog *)
  let cursor_options = [ Cursor.Options.Bulk; Cursor.Options.Append ] in
  let cursor =
    match
      Cursor.make (Catalog.session Catalog.instance) meta.name cursor_options
    with
    | Ok cursor -> cursor
    | _ -> failwith @@ "Failed opening a cursor for table: " ^ meta.name
  in
  BulkInserter { child_op; meta; cursor }

let open_op fs inserter = fs.open_op inserter.child_op

let close_op fs inserter =
  fs.close_op inserter.child_op;
  match Cursor.close inserter.cursor with
  | Ok () -> ()
  | _ -> failwith "Failed closing the cursor"

let next fs ctx inserter =
  let rec write_next () =
    let try_append record_data =
      let open Result in
      let buffer = Cursor.ValueBuffer.init_from_value record_data in
      let () = Cursor.set_value_from_buffer inserter.cursor buffer in
      let* () = Cursor.insert inserter.cursor in
      return ()
    in
    match fs.next ctx inserter.child_op with
    | Some (_, record_data) -> (
        match try_append record_data with
        | Ok () -> write_next ()
        | _ -> failwith "Failed inserting data into table")
    | None -> None
  in
  write_next ()
