open Core
open Storage
open Utils

type t = {
  meta : TableMeta.t;
  mutable tbls : TableMeta.t list;
  session : Storage.Database.Session.t;
}

let name = "LegoDB_catalog"

let create_tbl catalog (tbl_meta : TableMeta.t) =
  catalog.tbls <- tbl_meta :: catalog.tbls

let drop_tbl catalog (tbl_meta : TableMeta.t) =
  catalog.tbls <-
    List.filter ~f:(fun meta -> not @@ phys_equal meta tbl_meta) catalog.tbls

let make () =
  let session = Database.make () in
  let meta =
    TableMeta.make ~name
      ~schema:
        [
          Schema.Iu.make ~table:name ~column:"name" ~ty:(VarChar 128);
          Schema.Iu.make ~table:name ~column:"schema" ~ty:(VarChar 128);
        ]
      ~indexes:[ Index.PrimaryIdx [ "name" ] ]
      ()
  in
  let tbls =
    (* TODO: with a branch stage? *)
    if not (Database.Session.Crud.Table.exists session meta.name) then (
      ignore (Database.Session.Crud.Table.create session meta.name);
      [])
    else
      (* Need to build an execution tree and read data from it *)
      meta.name
      |> Database.Session.Crud.Record.read_all session
      |> Sequence.map ~f:(fun (id, data) ->
             let meta = TableMeta.Marshaller.unmarshal data in
             meta.tid <- id;
             meta)
      |> Sequence.to_list
  in
  { meta; tbls; session }

let instance = make ()
let meta catalog = catalog.meta
let session catalog = catalog.session
let tbls catalog = catalog.meta :: catalog.tbls

let find_tbl catalog tbl_name =
  let tbl_meta =
    List.find
      ~f:(fun tbl_meta -> String.equal tbl_name tbl_meta.name)
      (tbls catalog)
  in
  match tbl_meta with
  | Some meta -> meta
  | None -> failwith @@ "Table with name " ^ tbl_name ^ " not found"
