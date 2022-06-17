open Core

type t = { meta : Table.T.Meta.t; mutable tbls : Table.T.Meta.t list }

let name = "LegoDB_catalog"

(* TODO: define mli *)
let create_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.T.Crud.Tbl.create session_ref tbl_meta;
  Table.T.Crud.Record.insert session_ref catalog.meta
    (Table.T.Meta.Marshaller.marshal tbl_meta);
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <- tbl_meta :: catalog.tbls

let drop_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.T.Crud.Tbl.drop session_ref tbl_meta;
  Table.T.Crud.Record.delete session_ref catalog.meta
    (Table.T.Meta.Marshaller.marshal tbl_meta);
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <-
    List.filter ~f:(fun meta -> not @@ phys_equal meta tbl_meta) catalog.tbls

let create session_ref =
  let meta =
    Table.T.Meta.make name
      [ ("name", VarChar 128); ("schema", VarChar 128) ]
      [ Index.PrimaryIdx [ "name" ] ]
  in
  let tbls =
    if not (Table.T.Crud.Tbl.exists session_ref meta) then (
      Table.T.Crud.Tbl.create session_ref meta;
      [])
    else
      meta
      |> Table.T.Crud.Record.read_all session_ref
      |> Sequence.map ~f:Table.T.Meta.Marshaller.unmarshal
      |> Sequence.to_list
  in
  { meta; tbls }

let tbls catalog = catalog.meta :: catalog.tbls

let find_table catalog tbl_name =
  let tbl_meta =
    List.find
      ~f:(fun tbl_meta -> String.equal tbl_name @@ Table.T.Meta.name tbl_meta)
      (tbls catalog)
  in
  match tbl_meta with
  | Some meta -> meta
  | None -> failwith @@ "Table with name " ^ tbl_name ^ " not found"
