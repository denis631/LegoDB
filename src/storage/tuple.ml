open BatteriesExceptionless

type t = Value.t list

let eq lhs rhs = not @@ List.exists2 (neg2 Value.eq) lhs rhs

let parse schema data =
  data
  |> String.split_on_char '|'
  |> List.map2 Value.parse (List.map snd schema)


let hash t = List.map Value.hash t |> List.reduce Int64.logxor

let get = List.nth

let take = List.take

let extract_values idxs = List.filteri (fun i _ -> List.exists (( = ) i) idxs)

let show vals = String.concat " | " @@ List.map Value.show vals
