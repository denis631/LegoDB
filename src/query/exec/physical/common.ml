open Utils

type op = ..

module Record = Storage.Record
module RecordBuffer = Storage.Record_buffer

type interface = {
  output_schema : op -> Schema.t;
  open_op : op -> unit;
  close_op : op -> unit;
  next : unit -> op -> Record.t option;
}
