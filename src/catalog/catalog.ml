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
  (* Wired_tiger.Txn.begin_txn catalog.session; *)
  Database.Session.Crud.Table.create catalog.session tbl_meta.name;
  Database.Session.Crud.Record.insert catalog.session catalog.meta.name
    (Record.Id.zero, TableMeta.Marshaller.marshal tbl_meta);
  (* Wired_tiger.Txn.commit_txn catalog.session; *)
  catalog.tbls <- tbl_meta :: catalog.tbls

let drop_tbl catalog (tbl_meta : TableMeta.t) =
  (* Wired_tiger.Txn.begin_txn catalog.session; *)
  Database.Session.Crud.Table.drop catalog.session tbl_meta.name;

  (* TODO: find key for value? *)
  Database.Session.Crud.Record.delete catalog.session catalog.meta.name
    Record.Id.zero;

  (* Wired_tiger.Txn.commit_txn catalog.session; *)
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
      Database.Session.Crud.Table.create session meta.name;
      [])
    else
      (* Need to build an execution tree and read data from it *)

      meta.name
      |> Database.Session.Crud.Record.read_all session
      |> Sequence.map ~f:snd
      |> Sequence.map ~f:TableMeta.Marshaller.unmarshal
      |> Sequence.to_list
  in
  { meta; tbls; session }

let instance = make ()
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
