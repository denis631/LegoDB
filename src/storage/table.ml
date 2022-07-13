open Utils
module T = Tuple

type record = Tuple.t

open Core

module Meta = struct
  type t = { name : string; schema : Schema.t; indexes : Index.t list }
  [@@deriving make, show]

  type meta = t

  module Marshaller : Marshaller with type t = meta and type v = record = struct
    type t = meta
    type v = record

    let marshal meta : record = Tuple_buffer.marshal meta
    let unmarshal record : meta = Tuple_buffer.unmarshal record 0
  end
end

module Crud = struct
  let to_key (meta : Meta.t) record =
    let primary_key_columns : Schema.t =
      meta.indexes
      |> List.map ~f:(function Index.PrimaryIdx cols -> cols)
      |> List.concat
      |> List.map ~f:(fun idx ->
             List.find_exn meta.schema ~f:(fun (iu : Schema.Iu.t) ->
                 String.equal iu.column idx))
    in
    let output_buffer =
      Tuple_buffer.make @@ Tuple_buffer.length_from_schema primary_key_columns
    in
    T.copy_tuple record meta.schema output_buffer primary_key_columns;
    output_buffer

  module Tbl = struct
    let exists session_ref (meta : Meta.t) =
      Wired_tiger.Table.exists ~session_ref ~tbl_name:meta.name

    let create session_ref (meta : Meta.t) =
      Wired_tiger.Table.create ~session_ref ~tbl_name:meta.name
        ~config:"key_format:u,value_format:u"

    let drop session_ref (meta : Meta.t) =
      Wired_tiger.Table.drop ~session_ref ~tbl_name:meta.name ~config:""
  end

  module Record = struct
    let insert session_ref (meta : Meta.t) record =
      Wired_tiger.Record.insert_one ~session_ref ~tbl_name:meta.name
        ~key:(to_key meta record) ~record

    let bulk_insert session_ref meta records =
      let data = Sequence.map ~f:(fun r -> (to_key meta r, r)) records in
      Wired_tiger.Record.bulk_insert ~session_ref ~tbl_name:meta.name
        ~keys_and_records:(Sequence.to_list data)

    let read_all session_ref (meta : Meta.t) =
      let scanner = Wired_tiger.Record.scan ~session_ref ~tbl_name:meta.name in
      let generator f =
        match f () with Some record -> Some (record, f) | None -> None
      in
      Sequence.unfold ~init:scanner ~f:generator

    let delete session_ref (meta : Meta.t) record =
      Wired_tiger.Record.delete_one ~session_ref ~tbl_name:meta.name
        ~key:(to_key meta record)
  end
end
