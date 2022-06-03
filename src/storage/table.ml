module type Record = sig
  type t

  val marshal : t -> Wired_tiger.Record.t
  val unmarshal : Wired_tiger.Record.t -> t
end

module type Tbl = sig
  (* TODO: introduce Meta module *)
  type meta
  type record

  module Iu : sig
    type t

    val make : string -> string -> Value_type.t -> t
    val eq : t -> t -> bool
    val show : t -> string
  end

  module Iter : sig
    type t

    val make : Wired_tiger.session_ref -> meta -> t
    val next : t -> record option
    val to_list : t -> record list
  end

  val name : meta -> string
  val schema : meta -> Schema.t
  val create_meta : string -> Schema.t -> to_key:(record -> record) -> meta

  (* TODO: introduce a CRUD module *)
  val exists : Wired_tiger.session_ref -> meta -> bool
  val create : Wired_tiger.session_ref -> meta -> unit
  val insert : Wired_tiger.session_ref -> meta -> record -> unit
  val bulk_insert : Wired_tiger.session_ref -> meta -> record list -> unit
  val ius : meta -> Iu.t list
end

module Make (R : Record) = struct
  type meta = { name : string; mutable schema : Schema.t; to_key : R.t -> R.t }
  type record = R.t

  module Iu = struct
    type t = string * Schema.column_name * Value_type.t

    let make name col t = (name, col, t)
    let eq a b = a = b
    let show (_, col, ty) = "col: " ^ col ^ " | type: " ^ Value_type.show ty
  end

  module Iter = struct
    type t = unit -> Wired_tiger.Record.t option

    let make session_ref tbl =
      Wired_tiger.Record.scan ~session_ref ~tbl_name:tbl.name

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

  let name tbl = tbl.name
  let schema tbl = tbl.schema
  let create_meta name schema ~to_key = { name; schema; to_key }

  let exists session_ref meta =
    Wired_tiger.Table.exists ~session_ref ~tbl_name:meta.name

  let create session_ref meta =
    Wired_tiger.Table.create ~session_ref ~tbl_name:meta.name
      ~config:"key_format:u,value_format:u"

  let insert session_ref tbl record =
    Wired_tiger.Record.insert_one ~session_ref ~tbl_name:tbl.name
      ~key:(R.marshal @@ tbl.to_key record)
      ~record:(R.marshal record)

  let bulk_insert session_ref tbl records =
    let map f list =
      let rec loop acc = function
        | [] -> List.rev acc
        | x :: xs -> loop (f x :: acc) xs
      in
      loop [] list
    in
    let data =
      map (fun r -> (R.marshal @@ tbl.to_key r, R.marshal r)) records
    in
    Wired_tiger.Record.bulk_insert ~session_ref ~tbl_name:tbl.name
      ~keys_and_records:data

  let ius tbl = List.map (fun (col, ty) -> Iu.make tbl.name col ty) tbl.schema
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
