open Storage

let db = Database.create ()

let load_tpcc () =
  (* helpers *)
  let map f list =
    let rec loop acc = function
      | [] ->
          List.rev acc
      | x :: xs ->
          loop (f x :: acc) xs
    in
    loop [] list
  in
  let parse_tbl db tbl_name tbl_schema tuples_raw =
    let tbl = Table.RegularTbl.Meta.make tbl_name tbl_schema (fun x -> x) in
    Database.create_tbl db tbl;
    let tuples_parsed = map (Tuple.parse tbl_schema) tuples_raw in
    Table.RegularTbl.Crud.bulk_insert
      (Database.db_session_ref db)
      tbl tuples_parsed
  in

  let table_infos =
    let warehouse : string * Schema.t =
      ( "warehouse"
      , [ ("w_id", Integer)
        ; ("w_name", VarChar 10)
        ; ("w_street_1", VarChar 20)
        ; ("w_street_2", VarChar 20)
        ; ("w_city", VarChar 20)
        ; ("w_state", Char 2)
        ; ("w_zip", Char 9)
        ; ("w_tax", Numeric (4, 4))
        ; ("w_ytd", Numeric (12, 2))
        ] )
    in
    let district : string * Schema.t =
      ( "district"
      , [ ("d_id", Integer)
        ; ("d_w_id", Integer)
        ; ("d_name", VarChar 10)
        ; ("d_street_1", VarChar 20)
        ; ("d_street_2", VarChar 20)
        ; ("d_city", VarChar 20)
        ; ("d_state", Char 2)
        ; ("d_zip", Char 9)
        ; ("d_tax", Numeric (4, 4))
        ; ("d_ytd", Numeric (12, 2))
        ; ("d_next_o_id", Integer)
        ] )
    in
    let customer : string * Schema.t =
      ( "customer"
      , [ ("c_id", Integer)
        ; ("c_d_id", Integer)
        ; ("c_w_id", Integer)
        ; ("c_first", VarChar 16)
        ; ("c_middle", Char 2)
        ; ("c_last", VarChar 16)
        ; ("c_street_1", VarChar 20)
        ; ("c_street_2", VarChar 20)
        ; ("c_city", VarChar 20)
        ; ("c_state", Char 2)
        ; ("c_zip", Char 9)
        ; ("c_phone", Char 16)
        ; ("c_since", Timestamp)
        ; ("c_credit", Char 2)
        ; ("c_credit_lim", Numeric (12, 2))
        ; ("c_discount", Numeric (4, 4))
        ; ("c_balance", Numeric (12, 2))
        ; ("c_ytd_paymenr", Numeric (12, 2))
        ; ("c_payment_cnt", Numeric (4, 0))
        ; ("c_delivery_cnt", Numeric (4, 0))
        ; ("c_data", VarChar 500)
        ] )
    in
    let history : string * Schema.t =
      ( "history"
      , [ ("h_c_id", Integer)
        ; ("h_c_d_id", Integer)
        ; ("h_c_w_id", Integer)
        ; ("h_d_id", Integer)
        ; ("h_w_id", Integer)
        ; ("h_date", Timestamp)
        ; ("h_amount", Numeric (6, 2))
        ; ("h_data", VarChar 24)
        ] )
    in
    let neworder : string * Schema.t =
      ( "neworder"
      , [ ("no_o_id", Integer); ("no_d_id", Integer); ("no_w_id", Integer) ] )
    in
    let order : string * Schema.t =
      ( "order"
      , [ ("o_id", Integer)
        ; ("o_d_id", Integer)
        ; ("o_w_id", Integer)
        ; ("o_c_id", Integer)
        ; ("o_entry_d", Timestamp)
        ; ("o_carrier_id", Integer)
        ; ("o_ol_cnt", Numeric (2, 0))
        ; ("o_all_local", Numeric (1, 0))
        ] )
    in
    let orderline : string * Schema.t =
      ( "orderline"
      , [ ("ol_o_id", Integer)
        ; ("ol_d_id", Integer)
        ; ("ol_w_id", Integer)
        ; ("ol_number", Integer)
        ; ("ol_i_id", Integer)
        ; ("ol_supply_w_id", Integer)
        ; ("ol_delivery_d", Timestamp)
        ; ("ol_quantity", Numeric (2, 0))
        ; ("ol_amount", Numeric (6, 2))
        ; ("ol_dist_info", Char 24)
        ] )
    in
    let item : string * Schema.t =
      ( "item"
      , [ ("i_id", Integer)
        ; ("i_im_id", Integer)
        ; ("i_name", VarChar 24)
        ; ("i_price", Numeric (5, 2))
        ; ("i_data", VarChar 50)
        ] )
    in
    let stock : string * Schema.t =
      ( "stock"
      , [ ("s_i_id", Integer)
        ; ("s_w_id", Integer)
        ; ("s_quantity", Numeric (4, 0))
        ; ("s_dist_01", Char 24)
        ; ("s_dist_02", Char 24)
        ; ("s_dist_03", Char 24)
        ; ("s_dist_04", Char 24)
        ; ("s_dist_05", Char 24)
        ; ("s_dist_06", Char 24)
        ; ("s_dist_07", Char 24)
        ; ("s_dist_08", Char 24)
        ; ("s_dist_09", Char 24)
        ; ("s_dist_10", Char 24)
        ; ("s_ytd", Numeric (8, 0))
        ; ("s_order_cnt", Numeric (4, 0))
        ; ("s_remote_cnt", Numeric (4, 0))
        ; ("s_data", VarChar 50)
        ] )
    in
    [ warehouse
    ; district
    ; customer
    ; history
    ; neworder
    ; order
    ; orderline
    ; item
    ; stock
    ]
  in
  let read_file path =
    let chan = open_in path in
    let rec read acc =
      let read_line chan =
        try Some (input_line chan) with End_of_file -> None
      in
      match read_line chan with
      | Some line ->
          read @@ line :: acc
      | None ->
          List.rev acc
    in
    read []
  in
  List.iter
    (fun (name, schema) ->
      let file_contents = read_file @@ "data/tpcc_" ^ name ^ ".tbl" in
      parse_tbl db name schema file_contents )
    table_infos


let bootstrap = load_tpcc
