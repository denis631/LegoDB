open Common
open Core
open Ctypes

type selection = {
  predicate : (char ptr -> bool) Lazy.t;
  child_op : op;
  input_schema : Storage.Schema.t;
}

type op += Selection of selection

let make fs ~child_op ~predicate =
  let input_schema = fs.output_schema child_op in
  let predicate =
    let func () =
      let ctx = Vm.Ctx.make () in
      let instructions = Vm.Instruction.of_match_expr ctx predicate in
      Vm.compile ctx input_schema instructions
    in
    Lazy.from_fun func
  in
  Selection { predicate; child_op; input_schema }

let has_iu root_has_iu iu selection = root_has_iu iu selection.child_op

let open_op fs selection =
  (* Compile the predicate function *)
  ignore @@ Lazy.force_val selection.predicate;
  fs.open_op selection.child_op

let close_op fs selection = fs.close_op selection.child_op

let next fs ctx selection =
  let rec probe () =
    match fs.next ctx selection.child_op with
    | Some tuple ->
        let f = Lazy.force_val selection.predicate in
        if f @@ Storage.Tuple_buffer.to_ptr tuple then Some tuple else probe ()
    | None -> None
  in
  probe ()
