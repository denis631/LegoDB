open Core

type t = { meta : Table.T.Meta.t; mutable tbls : Table.T.Meta.t list }

let name = "__catalog"

let create_tbl catalog session_ref tbl_meta =
  Wired_tiger.Txn.begin_txn session_ref;
  Table.T.Crud.create session_ref tbl_meta;
  Table.T.Crud.insert session_ref catalog.meta
    [
      Value.VarChar (Table.T.Meta.name tbl_meta);
      Value.VarChar (Table.T.Meta.schema tbl_meta |> Schema.show);
    ];
  Wired_tiger.Txn.commit_txn session_ref;
  catalog.tbls <- tbl_meta :: catalog.tbls

let create session_ref =
  let meta = Table.T.Meta.make name [] (fun t -> t) in
  let tbls =
    if not (Table.T.Crud.exists session_ref meta) then (
      Table.T.Crud.create session_ref meta;
      [])
    else
      let tuple_to_table_meta tuple =
        Table.T.Meta.make
          (List.hd_exn tuple |> Value.show)
          (Schema.of_string @@ (List.tl_exn tuple |> List.hd_exn |> Value.show))
          (fun x -> x)
      in
      meta
      |> Table.T.Crud.read_all session_ref
      |> Sequence.map ~f:tuple_to_table_meta
      |> Sequence.to_list
  in
  { meta; tbls }

let tbls catalog = catalog.tbls
