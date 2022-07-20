open Common
open Core
open Storage
open Utils

type record_parser = {
  path : string;
  sep : Char.t;
  schema : Schema.t;
  mutable chan : In_channel.t option;
  buffer : RecordBuffer.t;
}

type op += RecordParser of record_parser

let make ~path ~schema =
  let sep =
    match String.sub path ~pos:(String.length path - 3) ~len:3 with
    | "tbl" -> '|'
    (* NOTE: it should be comma, but due to one dataset it is currently semicolon, allow it to be injectable *)
    | "csv" -> ';'
    | _ ->
        failwith
          "Unknown format. Don't know what is the separtor for it in order to \
           parse"
  in
  RecordParser
    {
      path;
      sep;
      schema;
      chan = None;
      buffer = RecordBuffer.make @@ RecordBuffer.length_from_schema schema;
    }

let open_op _ record_parser =
  record_parser.chan <- Some (In_channel.create record_parser.path)

let close_op _ record_parser =
  Option.iter ~f:In_channel.close record_parser.chan;
  record_parser.chan <- None

let next _ _ record_parser =
  let open Option in
  let get_line () = record_parser.chan >>= In_channel.input_line in
  let parse s =
    Record.Data.parse record_parser.schema ~sep:record_parser.sep
      record_parser.buffer s;
    (Record.Id.zero, record_parser.buffer)
  in
  get_line () >>| parse
