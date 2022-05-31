open Ctypes

let t = Bindings.item_t
let alloc = allocate t
let alloc_ptr () = allocate (ptr t) (from_voidp t null)
let get_data item = getf item Bindings.Item.data
let get_size item = getf item Bindings.Item.size
let set_data item = setf item Bindings.Item.data
let set_size item = setf item Bindings.Item.size

let of_bytes data =
  let item = make t in
  set_data item (bigarray_start array1 data |> to_voidp);
  set_size item (Unsigned.Size_t.of_int @@ Bytearray.length data);
  item

let to_bytes item =
  let data = get_data item |> from_voidp char in
  let size = get_size item |> Unsigned.Size_t.to_int in
  bigarray_of_ptr array1 size Bigarray.char data
