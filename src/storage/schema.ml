type column_name = string
type column = column_name * Value_type.t

(* TODO: should it be an iu instead? *)
type t = column list

let of_string s : t =
  let column_of_string s =
    match String.split_on_char '|' s with
    | c :: t :: _ -> (c, Value_type.of_string t)
    | _ -> failwith ""
  in

  s |> String.split_on_char ',' |> List.map column_of_string

let show t =
  String.concat "," @@ List.map (fun (c, t) -> c ^ "|" ^ Value_type.show t) t
