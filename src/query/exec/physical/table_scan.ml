open Common
open Core
open Storage
open Utils

module Cursor = Database.Session.Cursor
module TableMeta = Table_meta

module Result = struct
  include Result

  let ( let* ) r f = Result.bind r ~f
end

type tbl_scan = {
  meta : TableMeta.t;
  mutable cursor : Cursor.t option;
  buffer : Cursor.Buffer.t;
  mutable first_run : bool;
}

type op += TableScan of tbl_scan

let make ~(meta : TableMeta.t) =
  TableScan
    { meta; cursor = None; buffer = Cursor.Buffer.make (); first_run = true }

let has_iu _ iu tbl_scan =
  List.exists ~f:(Schema.Iu.equal iu) tbl_scan.meta.schema

let open_op _ ctx tbl_scan =
  let cursor =
    match Cursor.make ctx.session tbl_scan.meta.name [] with
    | Ok cursor -> cursor
    | _ ->
        failwith @@ "Failed opening a cursor for table: " ^ tbl_scan.meta.name
  in
  tbl_scan.cursor <- Some cursor;
  ()

(* Close the cursor and release the resources *)
let close_op _ _ tbl_scan =
  let cursor = Option.value_exn tbl_scan.cursor in
  match Cursor.close cursor with
  | Ok () -> tbl_scan.cursor <- None
  | _ -> failwith "Failed closing the cursor"

let next _ _ tbl_scan =
  let cursor = Option.value_exn tbl_scan.cursor in
  let get_next_record () =
    let open Result in
    let* () =
      (* If we are in the first run then seek to the first record, else go next *)
      if not tbl_scan.first_run then Cursor.next cursor
      else
        let minKey = Unsigned.UInt64.of_int 1 in
        Cursor.set_key cursor minKey;
        Cursor.seek cursor
    in
    tbl_scan.first_run <- false;
    let* () = Cursor.get_key_into_buffer cursor tbl_scan.buffer in
    let* () = Cursor.get_value_into_buffer cursor tbl_scan.buffer in
    return
      ( Cursor.Buffer.get_key tbl_scan.buffer,
        Cursor.Buffer.get_value tbl_scan.buffer )
  in
  match get_next_record () with Ok record -> Some record | _ -> None
