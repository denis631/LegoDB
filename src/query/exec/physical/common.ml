type op = ..

module Row = Storage.Row
module RowBuffer = Storage.Row_buffer

type interface = {
  output_schema : op -> Storage.Schema.t;
  open_op : op -> unit;
  close_op : op -> unit;
  next : unit -> op -> Storage.Row.t option;
}
