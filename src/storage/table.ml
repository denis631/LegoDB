module type Record = sig
  type t

  val marshal : t -> Wired_tiger.Record.t
  val unmarshal : Wired_tiger.Record.t -> t
end

module type Tbl = sig
  type record

  module Meta : sig
    type t

    (* TODO: fix this disaster (to_key) and what about auto incrementing ids, so no specific record field *)
    val make : string -> Schema.t -> (record -> record) -> t
    val name : t -> string
    val schema : t -> Schema.t
    val to_key : t -> record -> record
  end

  module Iu : sig
    type t

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
    val show : t -> string
  end

  module Iter : sig
    type t

    val make : Wired_tiger.session_ref -> Meta.t -> t
    val next : t -> record option
    val to_list : t -> record list
  end

  module Crud : sig
    val exists : Wired_tiger.session_ref -> Meta.t -> bool
    val create : Wired_tiger.session_ref -> Meta.t -> unit
    val insert : Wired_tiger.session_ref -> Meta.t -> record -> unit
    val bulk_insert : Wired_tiger.session_ref -> Meta.t -> record list -> unit
  end

  val ius : Meta.t -> Iu.t list
end

module Make (R : Record) = struct
  type record = R.t

  module Meta = struct
    type t = { name : string; schema : Schema.t; to_key : record -> record }

    let make name schema to_key = { name; schema; to_key }
    let name meta = meta.name
    let schema meta = meta.schema
    let to_key meta = meta.to_key
  end

  module Iu = struct
    type t = string * Schema.column_name * Value_type.t

    let make name col t = (name, col, t)
    let eq a b = a = b
    let show (_, col, ty) = "col: " ^ col ^ " | type: " ^ Value_type.show ty
  end

  module Iter = struct
    type t = unit -> Wired_tiger.Record.t option

    let make session_ref meta =
      Wired_tiger.Record.scan ~session_ref ~tbl_name:(Meta.name meta)

    let next iter =
      match iter () with
      | Some record -> (
          try Some (R.unmarshal record)
          with _ ->
            failwith "Data corruption happened. Cannot unmarshal the data")
      | None -> None

    let to_list iter =
      let rec f acc =
        match next iter with Some t -> f (t :: acc) | None -> List.rev acc
      in
      f []
  end

  module Crud = struct
    let exists session_ref meta =
      Wired_tiger.Table.exists ~session_ref ~tbl_name:(Meta.name meta)

    let create session_ref meta =
      Wired_tiger.Table.create ~session_ref ~tbl_name:(Meta.name meta)
        ~config:"key_format:u,value_format:u"

    let insert session_ref meta record =
      Wired_tiger.Record.insert_one ~session_ref ~tbl_name:(Meta.name meta)
        ~key:(R.marshal @@ Meta.to_key meta record)
        ~record:(R.marshal record)

    let bulk_insert session_ref meta records =
      let map f list =
        let rec loop acc = function
          | [] -> List.rev acc
          | x :: xs -> loop (f x :: acc) xs
        in
        loop [] list
      in
      let data =
        map (fun r -> (R.marshal @@ Meta.to_key meta r, R.marshal r)) records
      in
      Wired_tiger.Record.bulk_insert ~session_ref ~tbl_name:(Meta.name meta)
        ~keys_and_records:data
  end

  let ius meta =
    List.map
      (fun (col, ty) -> Iu.make (Meta.name meta) col ty)
      (Meta.schema meta)
end

module type RegularTbl = Tbl with type record = Tuple.t
module type CatalogTbl = Tbl with type record = string * Schema.t

module RegularTbl = Make (struct
  type t = Tuple.t

  let marshal obj = Bytearray.marshal obj []
  let unmarshal bytearray : t = Bytearray.unmarshal bytearray 0
end)

module CatalogTbl = Make (struct
  type t = string * Schema.t

  let marshal obj = Bytearray.marshal obj []
  let unmarshal bytearray : t = Bytearray.unmarshal bytearray 0
end)
