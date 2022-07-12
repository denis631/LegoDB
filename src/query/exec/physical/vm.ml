open Core
open Ctypes
open Foreign
open Expr
open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open Storage
open Utils

module Ctx = struct
  type t = {
    llctx : llcontext;
    llbuilder : llbuilder;
    llmodule : llmodule;
    varname_val_map : (string, llvalue) Stdlib.Hashtbl.t;
    mutable ius : Storage.Schema.t;
  }

  let make () =
    let llctx = create_context () in
    {
      llctx;
      llbuilder = builder llctx;
      llmodule = create_module llctx "func";
      varname_val_map = Stdlib.Hashtbl.create 10;
      ius = [];
    }
end

module Type = struct
  type ty =
    | Bool
    | Char
    | Int64
    | Array of ty * int
    | VarChar of int
    | StringLiteral

  let is_ptr = function Array _ -> true | VarChar _ -> true | _ -> false

  let of_value_type (ty : Value_type.t) =
    match ty with Integer -> Int64 | VarChar k -> VarChar k | _ -> failwith ""

  let to_c_type = function
    | Bool -> bool
    | _ -> failwith "this return type is currently not supported"

  let rec to_llvm_type llctx = function
    | Bool -> i1_type llctx
    | Char -> i8_type llctx
    | Int64 -> i64_type llctx
    | Array (t, k) -> array_type (to_llvm_type llctx t) k
    | VarChar _ -> pointer_type (to_llvm_type llctx Char)
    | StringLiteral -> failwith ""

  let rec equal lhs rhs =
    match (lhs, rhs) with
    | Bool, Bool -> true
    | Char, Char -> true
    | Int64, Int64 -> true
    | Array (lhs_t, lhs_k), Array (rhs_t, rhs_k) ->
        equal lhs_t rhs_t && Int.equal lhs_k rhs_k
    | VarChar lhs_k, VarChar rhs_k -> Int.equal lhs_k rhs_k
    | StringLiteral, VarChar _ -> true
    | VarChar _, StringLiteral -> true
    | StringLiteral, StringLiteral -> true
    | _ -> false
end

module Value = struct
  type t = Bool of bool | Int64 of int64 | StringLiteral of string
end

module Instruction = struct
  type func = {
    name : string;
    ret_type : Type.ty;
    args : Schema.Iu.t array;
    body : t list;
  }

  and t =
    | Const of Value.t
    | Var of string
    | Eq of t * t * Type.ty
    | If of t * t * t
    | FunCall of string * llvalue array

  let of_match_expr (ctx : Ctx.t) expr =
    let open Match in
    let args = ref [] in
    let func_body =
      let leaf_to_val = function
        | Expr.Leaf (Const (Integer i)) -> (Const (Value.Int64 i), Type.Int64)
        | Expr.Leaf (Const (Bool b)) -> (Const (Value.Bool b), Type.Bool)
        | Expr.Leaf (Const (StringLiteral s)) ->
            (Const (Value.StringLiteral s), Type.StringLiteral)
        | Expr.Leaf (TableAttr iu) ->
            (* Register IU as a new function argument *)
            args := iu :: !args;
            ctx.ius <- iu :: ctx.ius;
            (Var iu.column, Type.of_value_type iu.ty)
        | _ -> failwith ""
      in
      let rec to_instr = function
        | Expr.Eq (Leaf a, Leaf b) ->
            let lhs, lhs_ty = leaf_to_val (Expr.Leaf a) in
            let rhs, rhs_ty = leaf_to_val (Expr.Leaf b) in
            assert (Type.equal lhs_ty rhs_ty);
            Eq (lhs, rhs, lhs_ty)
        | Expr.And (x :: xs) ->
            let then_br = to_instr (Expr.And xs) in
            let else_br = Const (Value.Bool false) in
            If (to_instr x, then_br, else_br)
        | Expr.And [] -> Const (Value.Bool true)
        | _ -> failwith ""
      in
      to_instr expr
    in
    {
      name = "func";
      (* It's bool for Match.Boolean expression, but should support other
         types as well that could be used for projection, sorting, etc. *)
      ret_type = Type.Bool;
      args = Array.of_list !args;
      body = [ func_body ];
    }

  let rec codegen (ctx : Ctx.t) schema instr : llvalue =
    match instr with
    | Const (Bool b) ->
        const_int (Type.to_llvm_type ctx.llctx Type.Bool) (Bool.to_int b)
    | Const (Int64 i) ->
        const_int (Type.to_llvm_type ctx.llctx Type.Int64) (Int64.to_int_exn i)
    | Const (StringLiteral s) ->
        let const_str = const_stringz ctx.llctx s in
        let global_str = define_global s const_str ctx.llmodule in
        const_pointercast global_str
          (pointer_type @@ Type.to_llvm_type ctx.llctx Type.Char)
    | Var v -> Stdlib.Hashtbl.find ctx.varname_val_map v
    | Eq (lhs, rhs, ty) -> (
        let lhs = codegen ctx schema lhs in
        let rhs = codegen ctx schema rhs in
        match ty with
        | Int64 -> build_icmp Icmp.Eq lhs rhs "icmp_expr" ctx.llbuilder
        | VarChar _ ->
            let strcmp_result =
              codegen ctx schema (FunCall ("strcmp", [| lhs; rhs |]))
            in
            let zero = codegen ctx schema (Const (Value.Int64 0L)) in
            build_icmp Icmp.Eq strcmp_result zero "icmp_expr" ctx.llbuilder
        | _ -> failwith "not implemented yet")
    | If (cond_instr, then_instr, else_instr) ->
        let cond_val = codegen ctx schema cond_instr in

        (* Grab the first block so that we might later add the conditional branch
         * to it at the end of the function. *)
        let start_bb = insertion_block ctx.llbuilder in
        let func_val = block_parent start_bb in

        let context = ctx.llctx in
        let then_bb = append_block context "then" func_val in

        (* Emit 'then' value. *)
        position_at_end then_bb ctx.llbuilder;
        let then_val = codegen ctx schema then_instr in

        (* Codegen of 'then' can change the current block, update then_bb for the
         * phi. We create a new name because one is used for the phi node, and the
         * other is used for the conditional branch. *)
        let new_then_bb = insertion_block ctx.llbuilder in

        (* Emit 'else' value. *)
        let else_bb = append_block context "else" func_val in
        position_at_end else_bb ctx.llbuilder;
        let else_val = codegen ctx schema else_instr in

        (* Codegen of 'else' can change the current block, update else_bb for the
         * phi. *)
        let new_else_bb = insertion_block ctx.llbuilder in

        (* Emit merge block. *)
        let merge_bb = append_block context "ifcont" func_val in
        position_at_end merge_bb ctx.llbuilder;
        let incoming = [ (then_val, new_then_bb); (else_val, new_else_bb) ] in
        let phi = build_phi incoming "iftmp" ctx.llbuilder in

        (* Return to the start block to add the conditional branch. *)
        position_at_end start_bb ctx.llbuilder;
        ignore (build_cond_br cond_val then_bb else_bb ctx.llbuilder);

        (* Set a unconditional branch at the end of the 'then' block and the
         * 'else' block to the 'merge' block. *)
        position_at_end new_then_bb ctx.llbuilder;
        ignore (build_br merge_bb ctx.llbuilder);
        position_at_end new_else_bb ctx.llbuilder;
        ignore (build_br merge_bb ctx.llbuilder);

        (* Finally, set the builder to the end of the merge block. *)
        position_at_end merge_bb ctx.llbuilder;

        phi
    | FunCall (fun_name, args) ->
        let f = lookup_function fun_name ctx.llmodule |> Stdlib.Option.get in
        build_call f args "result" ctx.llbuilder

  let codegen_func (ctx : Ctx.t) schema f =
    let func_t =
      function_type
        (Type.to_llvm_type ctx.llctx f.ret_type)
        [| pointer_type (Type.to_llvm_type ctx.llctx Type.Char) |]
    in
    let func = declare_function "eval" func_t ctx.llmodule in

    (* Create the entry basic block and position the llbuilder there *)
    let bb = append_block ctx.llctx "entry" func in
    position_at_end bb ctx.llbuilder;

    (* Unpack all the function arguments by loading them from the tuple and
       register them in the var map *)
    Stdlib.Hashtbl.clear ctx.varname_val_map;
    let func_param = Array.get (params func) 0 in
    let load_elt (iu : Schema.Iu.t) =
      let addr =
        let offset = Storage.Schema.offset_to_attr schema iu in
        build_gep func_param
          [| const_int (i64_type ctx.llctx) offset |]
          iu.column ctx.llbuilder
      in
      let load name ty =
        let casted_ptr =
          let ptr_type =
            let t = Type.to_llvm_type ctx.llctx ty in
            if Type.is_ptr ty then t else pointer_type t
          in
          build_pointercast addr ptr_type name ctx.llbuilder
        in
        if Type.is_ptr ty then casted_ptr
          (* Derefence the pointer for the primitive types *)
        else build_load casted_ptr name ctx.llbuilder
      in
      load iu.column @@ Type.of_value_type iu.ty
    in
    Array.iter
      ~f:(fun (iu : Schema.Iu.t) ->
        Stdlib.Hashtbl.add ctx.varname_val_map iu.column (load_elt iu))
      f.args;

    (* Take the last value of the instruction and put it as the return value *)
    let ret_val = List.map ~f:(codegen ctx schema) f.body |> List.last_exn in

    (* Add the return statement to the func *)
    let _ = build_ret ret_val ctx.llbuilder in
    func
end

let compile (ctx : Ctx.t) schema func =
  let setup_llvm () =
    (* Perform the LLVM setup *)
    assert (Llvm_executionengine.initialize ());

    let triple = Target.default_triple () in
    let lltarget = Target.by_triple triple in
    let llmachine = TargetMachine.create ~triple lltarget in
    let lldly = TargetMachine.data_layout llmachine in

    set_target_triple (TargetMachine.triple llmachine) ctx.llmodule;
    set_data_layout (DataLayout.as_string lldly) ctx.llmodule
  in
  let setup_optimization_passes () =
    let fpm = PassManager.create_function ctx.llmodule in

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    add_instruction_combination fpm;

    (* reassociate expressions. *)
    add_reassociation fpm;

    (* Eliminate Common SubExpressions. *)
    add_gvn fpm;

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    add_cfg_simplification fpm;

    ignore (PassManager.initialize fpm);
    fpm
  in
  let declare_external_funcs () =
    let strcmp_t =
      let char_ptr = pointer_type (Type.to_llvm_type ctx.llctx Type.Char) in
      function_type
        (Type.to_llvm_type ctx.llctx Type.Int64)
        [| char_ptr; char_ptr |]
    in
    ignore (declare_function "strcmp" strcmp_t ctx.llmodule)
  in

  setup_llvm ();
  declare_external_funcs ();
  let pass_manager = setup_optimization_passes () in

  (* Perform codegen of the function *)
  let generated_func = Instruction.codegen_func ctx schema func in
  Llvm_analysis.assert_valid_module ctx.llmodule;

  (* Run optimizations over the generated function *)
  ignore (PassManager.run_function generated_func pass_manager);

  (* NOTE: For debugging. Remove later *)
  dump_module ctx.llmodule;

  (* Create the engine and get reference to the function *)
  let llengine = create ctx.llmodule in
  let fun_ptr =
    funptr (ptr char @-> returning (Type.to_c_type func.ret_type))
  in
  get_function_address "eval" fun_ptr llengine
