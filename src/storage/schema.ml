open Utils

type column_name = string
type column = column_name * Value_type.t

(* TODO: should it be an iu instead? *)
type t = column list
type schema = t

let show (t, _) =
  String.concat "," @@ List.map (fun (c, t) -> c ^ "|" ^ Value_type.show t) t

module Marshaller : Marshaller with type t = schema and type v = string = struct
  type t = schema
  type v = string

  (* TODO: marshal primary key as well *)
  let marshal t =
    String.concat "," @@ List.map (fun (c, t) -> c ^ "|" ^ Value_type.show t) t

  (* TODO: unmarshal primary key as well *)
  let unmarshal s =
    let column_of_string s =
      match String.split_on_char '|' s with
      | c :: t :: _ -> (c, Value_type.of_string t)
      | _ -> failwith ""
    in
    s |> String.split_on_char ',' |> List.map column_of_string
end
