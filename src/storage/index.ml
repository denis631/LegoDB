open Utils

type t = PrimaryIdx of Schema.column_name list
[@@deriving show { with_path = false }]

type index = t

module Marshaller : Marshaller with type t = index and type v = string = struct
  type t = index
  type v = string

  let marshal = function PrimaryIdx cols -> String.concat "," cols
  let unmarshal s = PrimaryIdx (String.split_on_char ',' s)
end
