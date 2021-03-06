open Core
open Utils

type t = Tuple_buffer.t

module Iterator = Tuple_buffer.Iterator

let parse schema ~sep buffer data =
  let types = List.map ~f:(fun (iu : Schema.Iu.t) -> iu.ty) schema in
  let iterator = Iterator.make buffer in
  let parse (ty : Value_type.t) str =
    let buffer_write = Iterator.write iterator in
    Value.parse_and_write ty str buffer_write
  in
  String.split ~on:sep data |> List.iter2_exn ~f:parse types

let copy_tuple src_buffer src_ius dst_buffer dst_ius =
  let dst_iter = Iterator.make dst_buffer in
  let copy_at_offset offset l =
    let src_iter = Iterator.make src_buffer in
    Iterator.advance_by_offset src_iter offset;
    Iterator.copy dst_iter src_iter l
  in
  let ls =
    List.map ~f:(fun (iu : Schema.Iu.t) -> Value_type.sizeof iu.ty) dst_ius
  in
  let offsets = List.map ~f:(Schema.offset_to_attr src_ius) dst_ius in
  List.iter2_exn offsets ls ~f:copy_at_offset

let show t schema =
  let iterator = Iterator.make t in
  List.map ~f:(fun (iu : Schema.Iu.t) -> Iterator.show iterator iu.ty) schema
  |> String.concat ~sep:","
