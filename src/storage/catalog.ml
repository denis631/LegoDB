type t = {
  meta : Table.CatalogTbl.meta;
  mutable tbls : Table.RegularTbl.meta list;
}

let name = "__catalog"

let create_tbl catalog session_ref tbl_meta =
  (* TODO: place in one txn *)
  Table.RegularTbl.create session_ref tbl_meta;
  Table.CatalogTbl.insert session_ref catalog.meta
    (Table.RegularTbl.name tbl_meta, Table.RegularTbl.schema tbl_meta);
  Printf.printf "created a table and wrote it to the catalog\n";
  catalog.tbls <- tbl_meta :: catalog.tbls

let create session_ref =
  let meta = Table.CatalogTbl.create_meta name [] ~to_key:(fun t -> t) in
  let tbls =
    if not (Table.CatalogTbl.exists session_ref meta) then (
      Table.CatalogTbl.create session_ref meta;
      [])
    else
      meta
      |> Table.CatalogTbl.Iter.make session_ref
      |> Table.CatalogTbl.Iter.to_list
      |> List.map (fun (name, schema) ->
             Table.RegularTbl.create_meta name schema ~to_key:(fun x -> x))
  in
  { meta; tbls }

let tbls catalog = catalog.tbls
