open Storage
open BatteriesExceptionless
open Common

type state =
  | BuildHashtbl
  | ProbeRight

type hash_join =
  { leftOp : op
  ; rightOp : op
  ; hash_tbl : (Storage.Tuple.t, Storage.Tuple.t * Table.Iu.t list) Hashtbl.t
  ; (* which of the attributes in the tuple will be used as key for lhs and rhs *)
    hash_key_ius : Table.Iu.t list * Table.Iu.t list
  ; mutable buffered_tuples : (Storage.Tuple.t * Table.Iu.t list) list
  ; mutable state : state
  }

type op += HashJoin of hash_join

let make ~leftOp ~rightOp ~hash_key_ius =
  HashJoin
    { leftOp
    ; rightOp
    ; hash_tbl = Hashtbl.create 101
    ; hash_key_ius
    ; buffered_tuples = []
    ; state = BuildHashtbl
    }


let prepare _ join = join

let to_hastbl_key ius (tuple, schema) =
  let required_for_key iu = List.exists (Table.Iu.eq iu) ius in
  let zip = List.map2 @@ curry identity in
  zip tuple schema |> List.filter (snd %> required_for_key) |> List.map fst


let build_hashtbl_if_needed root_next ctx join =
  let rec build () =
    let add_key_val result =
      let key = to_hastbl_key (fst join.hash_key_ius) result in
      Hashtbl.add join.hash_tbl key result
    in
    Option.may (add_key_val %> build) @@ root_next ctx join.leftOp
  in
  match join.state with
  | BuildHashtbl ->
      build () ;
      join.state <- ProbeRight
  | ProbeRight ->
      ()


let rec consume_tuple root_next ctx join =
  assert (join.state = ProbeRight) ;
  match join.buffered_tuples with
  | x :: xs ->
      join.buffered_tuples <- xs ;
      Some x
  | [] ->
      let probe_tbl (tuple, schema) =
        let results =
          let key = to_hastbl_key (snd join.hash_key_ius) (tuple, schema) in
          Hashtbl.find_all join.hash_tbl key
          |> List.map (fun (t, s) -> (tuple @ t, schema @ s))
        in
        join.buffered_tuples <- results ;
        consume_tuple root_next ctx join
      in
      let open Option.Infix in
      root_next ctx join.rightOp >>= probe_tbl


let next root_next ctx join =
  build_hashtbl_if_needed root_next ctx join ;
  consume_tuple root_next ctx join
