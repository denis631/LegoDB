open Core

type t = { meta : Table.T.Meta.t; mutable tbls : Table.T.Meta.t list }

let name = "__catalog"

let create_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.T.Crud.create session_ref tbl_meta;
  Table.T.Crud.insert session_ref catalog.meta
    (Table.T.Meta.Marshaller.marshal tbl_meta);
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <- tbl_meta :: catalog.tbls

let create session_ref =
  let meta = Table.T.Meta.make name ([], []) in
  let tbls =
    if not (Table.T.Crud.exists session_ref meta) then (
      Table.T.Crud.create session_ref meta;
      [])
    else
      meta
      |> Table.T.Crud.read_all session_ref
      |> Sequence.map ~f:Table.T.Meta.Marshaller.unmarshal
      |> Sequence.to_list
  in
  { meta; tbls }

let tbls catalog = catalog.tbls

let find_table catalog tbl_name =
  let tbl_meta =
    List.find
      ~f:(fun tbl_meta -> String.equal tbl_name @@ Table.T.Meta.name tbl_meta)
      catalog.tbls
  in
  match tbl_meta with
  | Some meta -> meta
  | None -> failwith @@ "Table with name " ^ tbl_name ^ " not found"
