type column_name = string
type column = column_name * Value_type.t

(* TODO: should it be an iu instead? *)
type t = column list
