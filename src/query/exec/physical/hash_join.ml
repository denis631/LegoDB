open Common
open Core
open Storage

module Option = struct
  include Option

  let unwrap = Stdlib.Option.get

  let with_fallback t ~fallback =
    match t with Some x -> Some x | None -> fallback ()
end

(* TODO: fix after refactoring *)
(* module Key = struct *)
(*   module T = struct *)
(*     type t = Storage.Tuple.t [@@deriving hash, compare, sexp] *)
(*   end *)
(**)
(*   include T *)
(*   include Hashable.Make (T) *)
(**)
(*   let of_tuple_schema ius (tuple, schema) = *)
(*     let required_for_key iu = List.exists ~f:(Storage.Schema.Iu.equal iu) ius in *)
(*     List.zip_exn tuple schema |> Sequence.of_list *)
(*     |> Sequence.filter ~f:(snd %> required_for_key) *)
(*     |> Sequence.map ~f:fst |> Sequence.to_list *)
(* end *)
(**)
(* module TupleHashtbl = Key.Table *)
(**)
(* type hash_tbl = (Storage.Tuple.t * Schema.t) list TupleHashtbl.t *)
(**)
type hash_join = {
  left_op : op;
  right_op : op;
  (* which of the attributes in the tuple will be used as key for lhs and rhs *)
  hash_key_ius : Schema.t * Schema.t;
  mutable hash_tbl : unit option;
  mutable buffered_tuples : (Storage.Tuple.t * Schema.t) list;
}

type op += HashJoin of hash_join

let make ~left_op ~right_op ~hash_key_ius =
  HashJoin
    { left_op; right_op; hash_key_ius; hash_tbl = None; buffered_tuples = [] }

let has_iu root_has_iu iu join =
  root_has_iu iu join.left_op || root_has_iu iu join.right_op

let open_op fs join =
  (* let build_hashtbl () = *)
  (*   join.hash_tbl <- Some (TupleHashtbl.create () ~size:101); *)
  (*   let rec build () = *)
  (*     let add_key_val result = *)
  (*       let key = Key.of_tuple_schema (fst join.hash_key_ius) result in *)
  (*       TupleHashtbl.add_multi (Option.unwrap join.hash_tbl) ~key ~data:result *)
  (*     in *)
  (*     Option.iter ~f:(add_key_val %> build) @@ fs.next () join.left_op *)
  (*   in *)
  (*   build () *)
  (* in *)
  fs.open_op join.left_op;
  (* build_hashtbl (); *)
  fs.close_op join.left_op;
  fs.open_op join.right_op

let close_op fs join =
  fs.close_op join.right_op;
  (* Clear the contents of the hash table as we don't need it anymore *)
  join.hash_tbl <- None

let next _ _ _ = None
(* let open Option in *)
(* let probe_tbl () = *)
(*   let lookup (tuple, schema) = *)
(*     let key = *)
(*       Key.of_tuple_schema (snd hash_join.hash_key_ius) (tuple, schema) *)
(*     in *)
(*     let results = *)
(*       TupleHashtbl.find_multi (Option.unwrap hash_join.hash_tbl) key *)
(*       |> List.map ~f:(fun (t, s) -> (tuple @ t, schema @ s)) *)
(*     in *)
(*     hash_join.buffered_tuples <- results; *)
(*     (\* By calling 'next' we either take tuple from the buffer, or the table is again probed *\) *)
(*     next fs ctx hash_join *)
(*   in *)
(*   fs.next ctx hash_join.right_op >>= lookup *)
(* in *)
(* match hash_join.buffered_tuples with *)
(* | x :: xs -> *)
(*     hash_join.buffered_tuples <- xs; *)
(*     Some x *)
(* | [] -> probe_tbl () *)
