open Common
open Core
open Storage
open Utils
module Cursor = Database.Session.Cursor

module Result = struct
  include Result

  let ( let* ) r f = Result.bind r ~f
end

type tbl_scan = {
  meta : TableMeta.t;
  cursor : Cursor.t;
  buffer : Cursor.ValueBuffer.t;
}

type op += TableScan of tbl_scan

let make ~(meta : TableMeta.t) =
  (* TODO: not sure the session should be tied by catalog *)
  let cursor =
    match Cursor.make (Catalog.session Catalog.instance) meta.name [] with
    | Ok cursor -> cursor
    | _ -> failwith @@ "Failed opening a cursor for table: " ^ meta.name
  in
  TableScan { meta; cursor; buffer = Cursor.ValueBuffer.make () }

let has_iu _ iu tbl_scan =
  List.exists ~f:(Schema.Iu.equal iu) tbl_scan.meta.schema

(* Create the cursor and move it to the first record *)
let open_op tbl_scan =
  let minKey = Unsigned.UInt64.of_int 1 in
  Cursor.set_key tbl_scan.cursor minKey;
  match Cursor.seek tbl_scan.cursor with
  | Ok () -> ()
  | _ -> failwith "Failed seeking to the first record in table"

(* Close the cursor and release the resources *)
let close_op tbl_scan =
  match Cursor.close tbl_scan.cursor with
  | Ok () -> ()
  | _ -> failwith "Failed closing the cursor"

let next _ tbl_scan =
  let get_next_record () =
    let open Result in
    let* () = Cursor.next tbl_scan.cursor in
    let* () = Cursor.get_key_into_buffer tbl_scan.cursor tbl_scan.buffer in
    let* () = Cursor.get_value_into_buffer tbl_scan.cursor tbl_scan.buffer in
    return
      ( Cursor.ValueBuffer.get_key tbl_scan.buffer,
        Cursor.ValueBuffer.get_value tbl_scan.buffer )
  in
  match get_next_record () with Ok record -> Some record | _ -> None
