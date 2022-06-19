open Common
open Core
open Storage

type row_parser = {
  path : string;
  sep : Char.t;
  schema : Schema.t;
  ius : Storage.Table.T.Iu.t list;
  mutable chan : In_channel.t option;
}

type op += RowParser of row_parser

let make ~path ~schema =
  let sep =
    match String.sub path ~pos:(String.length path - 3) ~len:3 with
    | "tbl" -> '|'
    (* NOTE: it should be comma, but due to one dataset it is currently semicolon *)
    | "csv" -> ';'
    | _ ->
        failwith
          "Unknown format. Don't know what is the separtor for it in order to \
           parse"
  in
  (* TODO: remove this once schema is a list of IUs *)
  let ius = List.map ~f:(fun (col, t) -> Table.T.Iu.make "" col t) schema in
  RowParser { path; sep; schema; ius; chan = None }

let open_op _ row_parser =
  row_parser.chan <- Some (In_channel.create row_parser.path)

let close_op _ row_parser =
  Option.iter ~f:In_channel.close row_parser.chan;
  row_parser.chan <- None

let next _ _ row_parser =
  let open Option in
  let get_line () = row_parser.chan >>= In_channel.input_line in
  let parse = Tuple.parse row_parser.schema ~sep:row_parser.sep in
  let to_result t = (t, row_parser.ius) in
  get_line () >>| parse >>| to_result
