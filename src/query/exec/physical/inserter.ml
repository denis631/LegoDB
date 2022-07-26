open Common
open Core
open Storage
open Utils
module Cursor = Database.Session.Cursor

type inserter = {
  child_op : op;
  meta : TableMeta.t;
  mutable cursor : Cursor.t option;
  is_bulk_insert : bool;
}

type op += Inserter of inserter

let make ~child_op ~(meta : TableMeta.t) ~is_bulk_insert =
  Inserter { child_op; meta; cursor = None; is_bulk_insert }

let open_op fs ctx inserter =
  fs.open_op ctx inserter.child_op;
  let cursor =
    let cursor_options =
      let open Cursor.Options in
      if inserter.is_bulk_insert then [ Bulk; Append ] else [ Append ]
    in
    match Cursor.make ctx.session inserter.meta.name cursor_options with
    | Ok cursor -> cursor
    | _ ->
        failwith @@ "Failed opening a cursor for table: " ^ inserter.meta.name
  in
  inserter.cursor <- Some cursor

let close_op fs ctx inserter =
  fs.close_op ctx inserter.child_op;
  let cursor = Option.value_exn inserter.cursor in
  match Cursor.close cursor with
  | Ok () -> inserter.cursor <- None
  | _ -> failwith "Failed closing the cursor"

let next fs ctx inserter =
  let try_append record_data =
    let cursor = Option.value_exn inserter.cursor in
    let buffer = Cursor.Buffer.init_from_value record_data in
    let () = Cursor.set_value_from_buffer cursor buffer in
    Cursor.insert cursor
  in
  let rec write_next () =
    match fs.next ctx inserter.child_op with
    | Some (_, record_data) -> (
        match try_append record_data with
        | Ok () -> write_next ()
        | _ -> failwith "Failed inserting data into table")
    | None -> None
  in
  write_next ()
