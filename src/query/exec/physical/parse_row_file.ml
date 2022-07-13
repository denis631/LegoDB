open Common
open Core
open Storage

type row_parser = {
  path : string;
  sep : Char.t;
  schema : Schema.t;
  mutable chan : In_channel.t option;
  buffer : Storage.Tuple_buffer.t;
}

type op += RowParser of row_parser

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
  RowParser
    {
      path;
      sep;
      schema;
      chan = None;
      buffer = Tuple_buffer.make @@ Tuple_buffer.length_from_schema schema;
    }

let open_op _ row_parser =
  row_parser.chan <- Some (In_channel.create row_parser.path)

let close_op _ row_parser =
  Option.iter ~f:In_channel.close row_parser.chan;
  row_parser.chan <- None

let next _ _ row_parser =
  let open Option in
  let get_line () = row_parser.chan >>= In_channel.input_line in
  let parse s =
    Tuple.parse row_parser.schema ~sep:row_parser.sep row_parser.buffer s;
    row_parser.buffer
  in
  get_line () >>| parse
