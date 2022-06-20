open Utils
module TupleMarshaller = Tuple.Marshaller

type tuple = Tuple.t

open Core

module type WiredTigerMarshaller = Marshaller with type v = Wired_tiger.Record.t

module type Tbl = sig
  type record = tuple

  module Meta : sig
    type t [@@deriving show]
    type meta = t

    val make : string -> Schema.t -> Index.t list -> t
    val name : t -> string
    val schema : t -> Schema.t
    val indexes : t -> Index.t list

    module Marshaller : Marshaller with type t = meta and type v = record
  end

  module Iu : sig
    type t [@@deriving show]

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
  end

  module Crud : sig
    module Tbl : sig
      val exists : Wired_tiger.session_ref -> Meta.t -> bool
      val create : Wired_tiger.session_ref -> Meta.t -> unit
      val drop : Wired_tiger.session_ref -> Meta.t -> unit
    end

    module Record : sig
      val insert : Wired_tiger.session_ref -> Meta.t -> record -> unit

      val bulk_insert :
        Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t -> unit

      val read_all : Wired_tiger.session_ref -> Meta.t -> record Core.Sequence.t
      val delete : Wired_tiger.session_ref -> Meta.t -> record -> unit
    end
  end

  val ius : Meta.t -> Iu.t list
end

module Make (M : WiredTigerMarshaller with type t = tuple) = struct
  type record = M.t

  module Meta = struct
    type t = { name : string; schema : Schema.t; indexes : Index.t list }
    [@@deriving show]

    type meta = t

    let make name schema indexes = { name; schema; indexes }
    let name meta = meta.name
    let schema meta = meta.schema
    let indexes meta = meta.indexes

    module Marshaller : Marshaller with type t = meta and type v = record =
    struct
      type t = meta
      type v = record

      let marshal meta : record =
        [
          Value.VarChar (name meta);
          Value.VarChar (schema meta |> Schema.Marshaller.marshal);
          Value.VarChar (indexes meta |> List.hd_exn |> Index.Marshaller.marshal);
        ]

      let unmarshal record : meta =
        match record with
        | [ name; schema; indexes ] ->
            make (Value.show name)
              (Schema.Marshaller.unmarshal @@ Value.show schema)
              [ Index.Marshaller.unmarshal @@ Value.show indexes ]
        | _ -> failwith "Cannot unmarshal the table meta"
    end
  end

  module Iu = struct
    type t = string * Schema.column_name * Value_type.t [@@deriving show]

    let make name col t = (name, col, t)
    let eq = Stdlib.( = )
  end

  module Crud = struct
    let to_key meta record =
      let columns = List.map ~f:fst @@ Meta.schema meta in
      let primary_key_columns =
        List.find_map_exn ~f:(function Index.PrimaryIdx cols -> Some cols)
        @@ Meta.indexes meta
      in
      let find_col_idx key_column =
        fst
        @@ List.findi_exn ~f:(fun _ elt -> String.( = ) elt key_column) columns
      in
      let extract_field_from_record_at_idx = List.nth_exn record in
      List.map
        ~f:(Fn.compose extract_field_from_record_at_idx find_col_idx)
        primary_key_columns

    module Tbl = struct
      let exists session_ref meta =
        Wired_tiger.Table.exists ~session_ref ~tbl_name:(Meta.name meta)

      let create session_ref meta =
        Wired_tiger.Table.create ~session_ref ~tbl_name:(Meta.name meta)
          ~config:"key_format:u,value_format:u"

      let drop session_ref meta =
        Wired_tiger.Table.drop ~session_ref ~tbl_name:(Meta.name meta)
          ~config:""
    end

    module Record = struct
      let insert session_ref meta record =
        Wired_tiger.Record.insert_one ~session_ref ~tbl_name:(Meta.name meta)
          ~key:(M.marshal @@ to_key meta record)
          ~record:(M.marshal record)

      let bulk_insert session_ref meta records =
        let data =
          Sequence.map
            ~f:(fun r -> (M.marshal @@ to_key meta r, M.marshal r))
            records
        in
        Wired_tiger.Record.bulk_insert ~session_ref ~tbl_name:(Meta.name meta)
          ~keys_and_records:(Sequence.to_list data)

      let read_all session_ref meta =
        let scanner =
          Wired_tiger.Record.scan ~session_ref ~tbl_name:(Meta.name meta)
        in
        let generator f =
          match f () with
          | Some record -> Some (M.unmarshal record, f)
          | None -> None
        in
        Sequence.unfold ~init:scanner ~f:generator

      let delete session_ref meta record =
        Wired_tiger.Record.delete_one ~session_ref ~tbl_name:(Meta.name meta)
          ~key:(M.marshal @@ to_key meta record)
    end
  end

  let ius meta =
    List.map
      ~f:(fun (col, ty) -> Iu.make (Meta.name meta) col ty)
      (Meta.schema meta)
end

module T = Make (TupleMarshaller)
