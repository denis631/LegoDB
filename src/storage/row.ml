open Core
open Utils

type t = Row_buffer.t

module Iterator = Row_buffer.Iterator

let parse schema ~sep buffer data =
  let types = List.map ~f:(fun (iu : Schema.Iu.t) -> iu.ty) schema in
  let iterator = Iterator.it_begin buffer in
  let parse (ty : Value_type.t) str =
    let buffer_write = Iterator.write iterator in
    Value.parse_and_write ty str buffer_write
  in
  String.split ~on:sep data |> List.iter2_exn ~f:parse types

let copy_to src_buffer src_ius dst_buffer dst_ius =
  let dst_iter = Iterator.it_begin dst_buffer in
  let copy_at_offset offset l =
    let src_iter = Iterator.it_begin src_buffer in
    Iterator.advance_by_offset src_iter offset;
    Iterator.copy dst_iter src_iter l
  in
  let ls =
    List.map ~f:(fun (iu : Schema.Iu.t) -> Value_type.sizeof iu.ty) dst_ius
  in
  let offsets = List.map ~f:(Schema.offset_to_attr src_ius) dst_ius in
  List.iter2_exn offsets ls ~f:copy_at_offset

let copy src_buffer src_ius dst_ius =
  let dst_buffer = Row_buffer.make @@ Row_buffer.length_from_schema dst_ius in
  copy_to src_buffer src_ius dst_buffer dst_ius;
  dst_buffer

let show t schema =
  let iterator = Iterator.it_begin t in
  List.map ~f:(fun (iu : Schema.Iu.t) -> Iterator.show iterator iu.ty) schema
  |> String.concat ~sep:","
