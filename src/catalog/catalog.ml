open Core
open Utils

module TableMeta = Utils.Table_meta

type t = { mutable tbls : TableMeta.t list }

let name = "LegoDB_catalog"

(* TODO: refactor this *)
let meta =
  TableMeta.make ~name
    ~schema:
      [
        Schema.Iu.make ~table:name ~column:"name" ~ty:(VarChar 128);
        Schema.Iu.make ~table:name ~column:"schema" ~ty:(VarChar 128);
      ]
    ~indexes:[ Index.PrimaryIdx [ "name" ] ]
    ()

let make () = { tbls = [] }
let set_catalog_tables catalog tbls = catalog.tbls <- tbls
let tbls catalog = meta :: catalog.tbls

let create_tbl catalog (tbl_meta : TableMeta.t) =
  catalog.tbls <- tbl_meta :: catalog.tbls

let drop_tbl catalog (tbl_meta : TableMeta.t) =
  catalog.tbls <-
    List.filter ~f:(fun meta -> not @@ phys_equal meta tbl_meta) catalog.tbls

let find_tbl catalog tbl_name =
  let tbl_meta =
    List.find
      ~f:(fun tbl_meta -> String.equal tbl_name tbl_meta.name)
      (tbls catalog)
  in
  match tbl_meta with
  | Some meta -> meta
  | None -> failwith @@ "Table with name " ^ tbl_name ^ " not found"
