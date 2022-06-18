open Common
open Core
open Storage
open Utils

module Key = struct
  module T = struct
    type t = Storage.Tuple.t [@@deriving hash, compare, sexp]
  end

  include T
  include Hashable.Make (T)
end

module TupleHashtbl = Key.Table

type hash_tbl = (Storage.Tuple.t * Table.T.Iu.t list) list TupleHashtbl.t
type state = BuildHashtbl | ProbeRight

type hash_join = {
  left_op : op;
  right_op : op;
  hash_tbl : hash_tbl;
  (* which of the attributes in the tuple will be used as key for lhs and rhs *)
  hash_key_ius : Table.T.Iu.t list * Table.T.Iu.t list;
  mutable buffered_tuples : (Storage.Tuple.t * Table.T.Iu.t list) list;
  mutable state : state;
}

type op += HashJoin of hash_join

let make ~left_op ~right_op ~hash_key_ius =
  HashJoin
    {
      left_op;
      right_op;
      hash_tbl = TupleHashtbl.create () ~size:101;
      hash_key_ius;
      buffered_tuples = [];
      state = BuildHashtbl;
    }

let has_iu root_has_iu iu join =
  root_has_iu iu join.left_op || root_has_iu iu join.right_op

let open_op fs join =
  fs.open_op join.left_op;
  fs.open_op join.right_op

let close_op fs join =
  (* TODO: after refactoring, the left will be closed in open_op *)
  fs.close_op join.left_op;
  fs.close_op join.right_op;
  (* Clear the contents of the hash table as we don't need it anymore *)
  TupleHashtbl.clear join.hash_tbl

let to_hastbl_key ius (tuple, schema) =
  let required_for_key iu = List.exists ~f:(Table.T.Iu.eq iu) ius in
  let zip = List.map2_exn ~f:(curry Fn.id) in
  zip tuple schema |> List.filter ~f:(snd %> required_for_key) |> List.map ~f:fst

let build_hashtbl_if_needed root_next ctx join =
  let rec build () =
    let add_key_val result =
      let key = to_hastbl_key (fst join.hash_key_ius) result in
      TupleHashtbl.add_multi join.hash_tbl ~key ~data:result
    in
    Option.iter ~f:(add_key_val %> build) @@ root_next ctx join.left_op
  in
  match join.state with
  | BuildHashtbl ->
      build ();
      join.state <- ProbeRight
  | ProbeRight -> ()

let rec consume_tuple root_next ctx join =
  assert (phys_equal join.state ProbeRight);
  match join.buffered_tuples with
  | x :: xs ->
      join.buffered_tuples <- xs;
      Some x
  | [] ->
      let probe_tbl (tuple, schema) =
        let results =
          let key = to_hastbl_key (snd join.hash_key_ius) (tuple, schema) in
          TupleHashtbl.find_multi join.hash_tbl key
          |> List.map ~f:(fun (t, s) -> (tuple @ t, schema @ s))
        in
        join.buffered_tuples <- results;
        consume_tuple root_next ctx join
      in
      let right_op = join.right_op in
      let open Option in
      root_next ctx right_op >>= probe_tbl

let next fs ctx join =
  let root_next = fs.next in
  build_hashtbl_if_needed root_next ctx join;
  consume_tuple root_next ctx join
