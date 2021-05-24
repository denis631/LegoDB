open Storage

let db = Database.create ()

let load_tpcc () =
  (* helpers *)
  let map f list =
    let rec loop acc = function
      | [] -> List.rev acc
      | x::xs -> loop (f x::acc) xs in
    loop [] list
  in
  let parse_tbl db tbl_name tbl_schema tuples_raw =
    let tbl = Table.create tbl_name tbl_schema in
    Database.insert db tbl;
    let tuples_parsed = map (Tuple.parse tbl_schema) tuples_raw in
    List.iter (Table.insert tbl) tuples_parsed
  in

  let table_infos =
    let warehouse: (String.t * Schema.t) = ("warehouse", [
        (Integer, "w_id");
        (VarChar 10, "w_name");
        (VarChar 20, "w_street_1");
        (VarChar 20, "w_street_2");
        (VarChar 20, "w_city");
        (Char 2, "w_state");
        (Char 9, "w_zip");
        (Numeric (4, 4), "w_tax");
        (Numeric (12, 2), "w_ytd");
      ])
    in
    let district: (String.t * Schema.t) = ("district", [
        (Integer, "d_id");
        (Integer, "d_w_id");
        (VarChar 10, "d_name");
        (VarChar 20, "d_street_1");
        (VarChar 20, "d_street_2");
        (VarChar 20, "d_city");
        (Char 2, "d_state");
        (Char 9, "d_zip");
        (Numeric (4, 4), "d_tax");
        (Numeric (12, 2), "d_ytd");
        (Integer, "d_next_o_id");
      ])
    in
    let customer: (String.t * Schema.t) = ("customer", [
        (Integer, "c_id");
        (Integer, "c_d_id");
        (Integer, "c_w_id");
        (VarChar 16, "c_first");
        (Char 2, "c_middle");
        (VarChar 16, "c_last");
        (VarChar 20, "c_street_1");
        (VarChar 20, "c_street_2");
        (VarChar 20, "c_city");
        (Char 2, "c_state");
        (Char 9, "c_zip");
        (Char 16, "c_phone");
        (Timestamp, "c_since");
        (Char 2, "c_credit");
        (Numeric (12, 2), "c_credit_lim");
        (Numeric (4, 4), "c_discount");
        (Numeric (12, 2), "c_balance");
        (Numeric (12, 2), "c_ytd_paymenr");
        (Numeric (4, 0), "c_payment_cnt");
        (Numeric (4, 0), "c_delivery_cnt");
        (VarChar 500, "c_data");
      ])
    in
    let history: (String.t * Schema.t) = ("history", [
        (Integer, "h_c_id");
        (Integer, "h_c_d_id");
        (Integer, "h_c_w_id");
        (Integer, "h_d_id");
        (Integer, "h_w_id");
        (Timestamp, "h_date");
        (Numeric (6, 2), "h_amount");
        (VarChar 24, "h_data");
      ])
    in
    let neworder: (String.t * Schema.t) = ("neworder", [
        (Integer, "no_o_id");
        (Integer, "no_d_id");
        (Integer, "no_w_id");
      ])
    in
    let order: (String.t * Schema.t) = ("order", [
        (Integer, "o_id");
        (Integer, "o_d_id");
        (Integer, "o_w_id");
        (Integer, "o_c_id");
        (Timestamp, "o_entry_d");
        (Integer, "o_carrier_id");
        (Numeric (2, 0), "o_ol_cnt");
        (Numeric (1, 0), "o_all_local");
      ])
    in
    let orderline: (String.t * Schema.t) = ("orderline", [
        (Integer, "ol_o_id");
        (Integer, "ol_d_id");
        (Integer, "ol_w_id");
        (Integer, "ol_number");
        (Integer, "ol_i_id");
        (Integer, "ol_supply_w_id");
        (Timestamp, "ol_delivery_d");
        (Numeric (2, 0), "ol_quantity");
        (Numeric (6, 2), "ol_amount");
        (Char 24, "ol_dist_info");
      ])
    in
    let item: (String.t * Schema.t) = ("item", [
        (Integer, "i_id");
        (Integer, "i_im_id");
        (VarChar 24, "i_name");
        (Numeric (5, 2),"i_price");
        (VarChar 50,"i_data");
      ])
    in
    let stock: (String.t * Schema.t) = ("stock", [
        (Integer, "s_i_id");
        (Integer, "s_w_id");
        (Numeric(4, 0) , "s_quantity");
        (Char 24, "s_dist_01");
        (Char 24, "s_dist_02");
        (Char 24, "s_dist_03");
        (Char 24, "s_dist_04");
        (Char 24, "s_dist_05");
        (Char 24, "s_dist_06");
        (Char 24, "s_dist_07");
        (Char 24, "s_dist_08");
        (Char 24, "s_dist_09");
        (Char 24, "s_dist_10");
        (Numeric (8, 0), "s_ytd");
        (Numeric (4, 0), "s_order_cnt");
        (Numeric (4, 0), "s_remote_cnt");
        (VarChar 50, "s_data");
      ])
    in
    [warehouse; district; customer; history; neworder; order; orderline; item; stock;]
  in
  let read_file path =
    let chan = open_in path in
    let rec read acc =
      let read_line chan = try Some (input_line chan) with End_of_file -> None in
      match read_line chan with
      | Some line -> read @@ line::acc
      | None -> List.rev acc
    in
    read []
  in
  List.iter (fun (name, schema) ->
      let file_contents = read_file @@ "data/tpcc_" ^ name ^ ".tbl" in
      parse_tbl db name schema file_contents
    )
    table_infos

let bootstrap () = load_tpcc ()
