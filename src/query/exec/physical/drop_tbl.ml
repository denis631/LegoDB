open Common
open Core
open Storage
open Utils
module Cursor = Database.Session.Cursor

module Result = struct
  include Result

  let ( let* ) r f = Result.bind r ~f
end

type drop_tbl = { metas : TableMeta.t list }
type op += DropTbl of drop_tbl

let make ~metas = DropTbl { metas }
let open_op _ _ = ()
let close_op _ _ = ()

let next _ _ drop_tbl =
  let session = Catalog.session Catalog.instance in
  let perform_writes (table_meta : TableMeta.t) =
    let open Result in
    let delete_from_catalog_table () =
      let catalog_meta = Catalog.meta Catalog.instance in
      let* cursor = Cursor.make session catalog_meta.name [] in
      Cursor.set_key cursor table_meta.tid;
      let* () = Cursor.remove cursor in
      let* () = Cursor.close cursor in
      return ()
    in
    let* () = Database.Session.Crud.Table.drop session table_meta.name in
    let* () = delete_from_catalog_table () in
    return ()
  in
  List.iter ~f:(fun m -> 
    match perform_writes m with
    | Ok () -> ()
    | _ -> failwith @@ "Failed removing table " ^ m.name
  ) drop_tbl.metas;
  None

