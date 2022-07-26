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
let open_op _ _ _ = ()
let close_op _ _ _ = ()

let next _ ctx drop_tbl =
  let perform_writes (table_meta : TableMeta.t) =
    let open Result in
    let delete_from_catalog_table () =
      let catalog_meta = Catalog.meta in
      let* cursor = Cursor.make ctx.session catalog_meta.name [] in
      Cursor.set_key cursor table_meta.tid;
      let* () = Cursor.remove cursor in
      let* () = Cursor.close cursor in
      return ()
    in
    let* () = Database.Session.Table.drop ctx.session table_meta.name in
    let* () = delete_from_catalog_table () in
    return ()
  in
  List.iter
    ~f:(fun m ->
      match perform_writes m with
      | Ok () -> Catalog.drop_tbl ctx.catalog m
      | _ -> failwith @@ "Failed removing table " ^ m.name)
    drop_tbl.metas;
  None
