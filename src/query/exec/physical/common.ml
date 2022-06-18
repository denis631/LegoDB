type op = ..

type interface = {
  open_op : op -> unit;
  close_op : op -> unit;
  next : unit -> op -> (Storage.Tuple.t * Storage.Table.T.Iu.t list) option;
}
