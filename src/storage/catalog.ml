type t = {
  meta : Table.RegularTbl.Meta.t;
  mutable tbls : Table.RegularTbl.Meta.t list;
}

let name = "__catalog"

let create_tbl catalog session_ref tbl_meta =
  (* TODO: place in one txn *)
  Table.RegularTbl.Crud.create session_ref tbl_meta;
  Table.RegularTbl.Crud.insert session_ref catalog.meta
    [
      Value.VarChar (Table.RegularTbl.Meta.name tbl_meta);
      Value.VarChar (Table.RegularTbl.Meta.schema tbl_meta |> Schema.show);
    ];
  Printf.printf "created a table and wrote it to the catalog\n";
  catalog.tbls <- tbl_meta :: catalog.tbls

let create session_ref =
  let meta = Table.RegularTbl.Meta.make name [] (fun t -> t) in
  let tbls =
    if not (Table.RegularTbl.Crud.exists session_ref meta) then (
      Table.RegularTbl.Crud.create session_ref meta;
      [])
    else
      let tuple_to_catalog_elt tuple =
        Table.RegularTbl.Meta.make
          (List.hd tuple |> Value.show)
          (Schema.of_string @@ (List.tl tuple |> List.hd |> Value.show))
          (fun x -> x)
      in
      meta
      |> Table.RegularTbl.Iter.make session_ref
      |> Table.RegularTbl.Iter.to_list
      |> List.map tuple_to_catalog_elt
  in
  { meta; tbls }

let tbls catalog = catalog.tbls
