type column_name = string

(* TODO: should it be an iu instead? *)
type t = (column_name * Value_type.t) list
