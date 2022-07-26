open Utils

type ctx = { session : Storage.Database.Session.t; catalog : Catalog.t }
type op = ..

module Record = Storage.Record
module RecordBuffer = Storage.Record_buffer

type interface = {
  output_schema : op -> Schema.t;
  open_op : ctx -> op -> unit;
  close_op : ctx -> op -> unit;
  next : ctx -> op -> Record.t option;
}
