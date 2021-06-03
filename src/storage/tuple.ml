type t = Value.t list

let parse schema data =
  data
  |> String.split_on_char '|'
  |> List.map2 Value.parse (List.map fst schema)


let show vals = String.concat " | " @@ List.map Value.show vals
