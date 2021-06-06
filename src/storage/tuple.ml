type t = Value.t list

let parse schema data =
  data
  |> String.split_on_char '|'
  |> List.map2 Value.parse (List.map snd schema)


let get = List.nth

let extract_values idxs = List.filteri (fun i _ -> List.exists (( = ) i) idxs)

let show vals = String.concat " | " @@ List.map Value.show vals
