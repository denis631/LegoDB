open Common
open Core
open Storage
open Utils
module Cursor = Database.Session.Cursor

module Result = struct
  include Result

  let ( let* ) r f = Result.bind r ~f
end

type create_tbl = { meta : TableMeta.t }
type op += CreateTbl of create_tbl

let make ~tbl_meta = CreateTbl { meta = tbl_meta }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ create_tbl =
  let session = Catalog.session Catalog.instance in
  let perform_writes () =
    let open Result in
    let write_into_catalog_table () =
      let catalog_meta = Catalog.meta Catalog.instance in
      let* cursor =
        Cursor.make session catalog_meta.name [ Cursor.Options.Append ]
      in
      let record_data = TableMeta.Marshaller.marshal create_tbl.meta in
      Cursor.set_value cursor record_data;
      let* () = Cursor.insert cursor in

      (* Update record id of the table for easy removal *)
      let* record_id =
        let buffer = Cursor.Buffer.make () in
        let* () = Cursor.get_key_into_buffer cursor buffer in
        return (Cursor.Buffer.get_key buffer)
      in
      create_tbl.meta.tid <- record_id;
      let* () = Cursor.close cursor in
      return ()
    in
    let* () = Database.Session.Crud.Table.create session create_tbl.meta.name in
    let* () = write_into_catalog_table () in
    return ()
  in
  match perform_writes () with
  | Ok () ->
      Catalog.create_tbl Catalog.instance create_tbl.meta;
      None
  | _ -> failwith "Failed creating a table"
