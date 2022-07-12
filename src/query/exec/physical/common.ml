type op = ..

type interface = {
  output_schema : op -> Storage.Schema.t;
  open_op : op -> unit;
  close_op : op -> unit;
  next : unit -> op -> Storage.Tuple.t option;
}
