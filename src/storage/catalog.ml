type t = {
  meta : Table.CatalogTbl.Meta.t;
  mutable tbls : Table.RegularTbl.Meta.t list;
}

let name = "__catalog"

let create_tbl catalog session_ref tbl_meta =
  (* TODO: place in one txn *)
  Table.RegularTbl.Crud.create session_ref tbl_meta;
  Table.CatalogTbl.Crud.insert session_ref catalog.meta
    (Table.RegularTbl.Meta.name tbl_meta, Table.RegularTbl.Meta.schema tbl_meta);
  Printf.printf "created a table and wrote it to the catalog\n";
  catalog.tbls <- tbl_meta :: catalog.tbls

let create session_ref =
  let meta = Table.CatalogTbl.Meta.make name [] (fun t -> t) in
  let tbls =
    if not (Table.CatalogTbl.Crud.exists session_ref meta) then (
      Table.CatalogTbl.Crud.create session_ref meta;
      [])
    else
      meta
      |> Table.CatalogTbl.Iter.make session_ref
      |> Table.CatalogTbl.Iter.to_list
      |> List.map (fun (name, schema) ->
             Table.RegularTbl.Meta.make name schema (fun x -> x))
  in
  { meta; tbls }

let tbls catalog = catalog.tbls
