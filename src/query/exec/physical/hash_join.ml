open Storage
open BatteriesExceptionless
open Common

module TupleHashtbl = Hashtbl.Make (struct
  type t = Storage.Tuple.t

  let equal = Storage.Tuple.eq
  let hash = Storage.Tuple.hash %> Int64.to_int
end)

type hash_tbl = (Storage.Tuple.t * Table.T.Iu.t list) TupleHashtbl.t
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
      hash_tbl = TupleHashtbl.create 101;
      hash_key_ius;
      buffered_tuples = [];
      state = BuildHashtbl;
    }

let has_iu root_has_iu iu join =
  root_has_iu iu join.left_op || root_has_iu iu join.right_op

let open_op f join =
  f join.left_op;
  f join.right_op

let to_hastbl_key ius (tuple, schema) =
  let required_for_key iu = List.exists (Table.T.Iu.eq iu) ius in
  let zip = List.map2 @@ curry identity in
  zip tuple schema |> List.filter (snd %> required_for_key) |> List.map fst

let build_hashtbl_if_needed root_next ctx join =
  let rec build () =
    let add_key_val result =
      let key = to_hastbl_key (fst join.hash_key_ius) result in
      TupleHashtbl.add join.hash_tbl key result
    in
    Option.may (add_key_val %> build) @@ root_next ctx join.left_op
  in
  match join.state with
  | BuildHashtbl ->
      build ();
      join.state <- ProbeRight
  | ProbeRight -> ()

let rec consume_tuple root_next ctx join =
  assert (join.state = ProbeRight);
  match join.buffered_tuples with
  | x :: xs ->
      join.buffered_tuples <- xs;
      Some x
  | [] ->
      let probe_tbl (tuple, schema) =
        let results =
          let key = to_hastbl_key (snd join.hash_key_ius) (tuple, schema) in
          TupleHashtbl.find_all join.hash_tbl key
          |> List.map (fun (t, s) -> (tuple @ t, schema @ s))
        in
        join.buffered_tuples <- results;
        consume_tuple root_next ctx join
      in
      let open Option.Infix in
      root_next ctx join.right_op >>= probe_tbl

let next root_next ctx join =
  build_hashtbl_if_needed root_next ctx join;
  consume_tuple root_next ctx join
