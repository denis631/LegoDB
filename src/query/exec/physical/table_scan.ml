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
  mutable first_run : bool;
}

type op += TableScan of tbl_scan

let make ~(meta : TableMeta.t) =
  (* TODO: not sure the session should be tied by catalog *)
  let cursor =
    match Cursor.make (Catalog.session Catalog.instance) meta.name [] with
    | Ok cursor -> cursor
    | _ -> failwith @@ "Failed opening a cursor for table: " ^ meta.name
  in
  TableScan
    { meta; cursor; buffer = Cursor.ValueBuffer.make (); first_run = true }

let has_iu _ iu tbl_scan =
  List.exists ~f:(Schema.Iu.equal iu) tbl_scan.meta.schema

let open_op _ = ()

(* Close the cursor and release the resources *)
let close_op tbl_scan =
  match Cursor.close tbl_scan.cursor with
  | Ok () -> ()
  | _ -> failwith "Failed closing the cursor"

let next _ tbl_scan =
  let get_next_record () =
    let open Result in
    let* () =
      (* If we are in the first run then seek to the first record, else go next *)
      if not tbl_scan.first_run then Cursor.next tbl_scan.cursor
      else
        let minKey = Unsigned.UInt64.of_int 1 in
        Cursor.set_key tbl_scan.cursor minKey;
        Cursor.seek tbl_scan.cursor
    in
    tbl_scan.first_run <- false;
    let* () = Cursor.get_key_into_buffer tbl_scan.cursor tbl_scan.buffer in
    let* () = Cursor.get_value_into_buffer tbl_scan.cursor tbl_scan.buffer in
    return
      ( Cursor.ValueBuffer.get_key tbl_scan.buffer,
        Cursor.ValueBuffer.get_value tbl_scan.buffer )
  in
  match get_next_record () with Ok record -> Some record | _ -> None
