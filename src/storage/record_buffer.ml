open Core
open Ctypes
open Utils
open Utils.Bindings

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type buffer = t

let make = Bytearray.create
let length = Bytearray.length

let length_from_schema schema =
  List.map ~f:(fun (iu : Schema.Iu.t) -> Value_type.sizeof iu.ty) schema
  |> List.reduce ~f:Int.( + ) |> Option.value ~default:0

let clone t =
  let out = make @@ length t in
  Bigarray.Array1.blit t out;
  out

let to_ptr = bigarray_start array1
let of_ptr ptr l = bigarray_of_ptr array1 l Bigarray.char ptr
let marshal x = Bytearray.marshal x []
let unmarshal = Bytearray.unmarshal

module Iterator = struct
  type t = char ptr ref

  let it_begin buffer = ref @@ to_ptr buffer

  let it_end buffer =
    let ptr = to_ptr buffer in
    let l = Bytearray.length buffer in
    ref @@ (ptr +@ l)

  let get_ptr t = !t

  let advance_by_offset t l =
    let cur_ptr = get_ptr t in
    t := cur_ptr +@ l

  let advance_by_type t (ty : Value_type.t) =
    advance_by_offset t @@ Value_type.sizeof ty

  let copy dst src l =
    let src_ptr = get_ptr src in
    let dst_ptr = get_ptr dst in
    ignore (memcpy dst_ptr src_ptr (Unsigned.Size_t.of_int l));
    advance_by_offset dst l

  let write t x =
    let dst_ptr = get_ptr t in
    match x with
    | Cvalue.Int i ->
        let int_ptr = coerce (ptr char) (ptr int64_t) dst_ptr in
        int_ptr <-@ i;
        advance_by_offset t 8
    | Cvalue.String s ->
        let l = String.length s in
        let open Unsigned.Size_t in
        ignore (memcpy_ocaml_string dst_ptr (ocaml_string_start s) (of_int l));
        advance_by_offset t l

  let show t (ty : Value_type.t) =
    let cur_ptr = get_ptr t in
    let result =
      match ty with
      | Integer ->
          let int_ptr = coerce (ptr char) (ptr int64_t) cur_ptr in
          Int64.to_string !@int_ptr
      | Numeric (_, precision) ->
          let int_ptr = coerce (ptr char) (ptr int64_t) cur_ptr in
          let x = !@int_ptr in
          let tmp = Int64.pow (Int64.of_int 10) (Int64.of_int precision) in
          Int64.to_string Int64.(x / tmp)
          ^ "."
          ^ Int64.to_string Int64.(x % tmp)
      | Char k -> string_from_ptr cur_ptr ~length:k
      | VarChar k -> string_from_ptr cur_ptr ~length:k
      | Timestamp ->
          let int_ptr = coerce (ptr char) (ptr int64_t) cur_ptr in
          Int64.to_string !@int_ptr
    in
    advance_by_type t ty;
    result
end
