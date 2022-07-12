open Core

type t = { meta : Table.Meta.t; mutable tbls : Table.Meta.t list }

let name = "LegoDB_catalog"

(* TODO: define mli *)
let create_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.Crud.Tbl.create session_ref tbl_meta;
  Table.Crud.Record.insert session_ref catalog.meta
    (Table.Meta.Marshaller.marshal tbl_meta);
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <- tbl_meta :: catalog.tbls

let drop_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.Crud.Tbl.drop session_ref tbl_meta;
  Table.Crud.Record.delete session_ref catalog.meta
    (Table.Meta.Marshaller.marshal tbl_meta);
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <-
    List.filter ~f:(fun meta -> not @@ phys_equal meta tbl_meta) catalog.tbls

let create session_ref =
  let meta =
    Table.Meta.make ~name
      ~schema:
        [
          Schema.Iu.make ~table:name ~column:"name" ~ty:(VarChar 128);
          Schema.Iu.make ~table:name ~column:"schema" ~ty:(VarChar 128);
        ]
      ~indexes:[ Index.PrimaryIdx [ "name" ] ]
      ()
  in
  let tbls =
    if not (Table.Crud.Tbl.exists session_ref meta) then (
      Table.Crud.Tbl.create session_ref meta;
      [])
    else
      meta
      |> Table.Crud.Record.read_all session_ref
      |> Sequence.map ~f:Table.Meta.Marshaller.unmarshal
      |> Sequence.to_list
  in
  { meta; tbls }

let tbls catalog = catalog.meta :: catalog.tbls

let find_table catalog tbl_name =
  let tbl_meta =
    List.find
      ~f:(fun tbl_meta -> String.equal tbl_name tbl_meta.name)
      (tbls catalog)
  in
  match tbl_meta with
  | Some meta -> meta
  | None -> failwith @@ "Table with name " ^ tbl_name ^ " not found"
