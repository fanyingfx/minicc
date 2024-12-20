open Utils
open Assembly

let param_passing_regs = Assembly.[ DI; SI; DX; CX; R8; R9 ]
let convert_val = function Tacky.Constant i -> Imm i | Tacky.Var v -> Pseudo v

let convert_op = function
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg
  | Tacky.Not ->
      failwith "Internal error, can't convert TACKY not directly to assembly"
      [@coverage off]

let convert_binop = function
  | Tacky.Add -> Add
  | Tacky.Multiply -> Mult
  | Tacky.Subtract -> Sub
  | Tacky.(
      ( Divide | Mod | Equal | NotEqual | GreaterOrEqual | LessOrEqual
      | GreaterThan | LessThan )) ->
      failwith "Internal error: not a binary assembly instruction"
      [@coverage off]

let convert_cond_code = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> Assembly.G
  | Tacky.GreaterOrEqual -> Assembly.GE
  | Tacky.LessThan -> Assembly.L
  | Tacky.LessOrEqual -> Assembly.LE
  | _ -> failwith "Internal error: not a condition code" [@coverage off]

let convert_function_call fun_name args dst =
  let reg_args, stack_args = ListUtil.take_drop 6 args in
  let stack_padding = if List.length stack_args mod 2 = 0 then 0 else 8 in
  let instructions =
    if stack_padding = 0 then [] else [ AllocateStack stack_padding ]
  in
  let pass_reg_arg idx arg =
    let r = List.nth param_passing_regs idx in
    let assembly_arg = convert_val arg in
    Mov (assembly_arg, Reg r)
  in
  let instructions = instructions @ List.mapi pass_reg_arg reg_args in
  let pass_stack_arg arg =
    let assembly_arg = convert_val arg in
    match assembly_arg with
    | Assembly.Imm _ | Reg _ -> [ Assembly.Push assembly_arg ]
    | _ -> [ Mov (assembly_arg, Reg AX); Push (Reg AX) ]
  in
  let instructions =
    instructions @ List.concat (List.rev_map pass_stack_arg stack_args)
  in
  let instructions = instructions @ [ Assembly.Call fun_name ] in

  let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
  let dealloc =
    if bytes_to_remove = 0 then [] else [ DeallocateStack bytes_to_remove ]
  in
  let instructions = instructions @ dealloc in
  let assembly_dst = convert_val dst in
  instructions @ [ Mov (Reg AX, assembly_dst) ]

let convert_instruction = function
  | Tacky.Return tacky_val ->
      let asm_val = convert_val tacky_val in
      [ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op = Not; dst; src } ->
      let asm_src = convert_val src and asm_dst = convert_val dst in
      [ Cmp (Imm 0, asm_src); Mov (Imm 0, asm_dst); SetCC (E, asm_dst) ]
  | Tacky.Unary { op; dst; src } ->
      let asm_op = convert_op op
      and asm_src = convert_val src
      and asm_dst = convert_val dst in
      [ Mov (asm_src, asm_dst); Unary (asm_op, asm_dst) ]
  | Tacky.Jump target -> [ Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let asm_cond = convert_val cond in
      [ Cmp (Imm 0, asm_cond); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let asm_cond = convert_val cond in
      [ Cmp (Imm 0, asm_cond); JmpCC (NE, target) ]
  | Tacky.Copy { src; dst } ->
      let asm_src = convert_val src and asm_dst = convert_val dst in
      [ Mov (asm_src, asm_dst) ]
  | Tacky.Label identifier -> [ Label identifier ]
  | Tacky.Binary { op; src1; src2; dst } -> (
      let asm_src1 = convert_val src1
      and asm_src2 = convert_val src2
      and asm_dst = convert_val dst in
      match op with
      | GreaterThan | GreaterOrEqual | LessThan | LessOrEqual | Equal | NotEqual
        ->
          let cond_code = convert_cond_code op in
          [
            Cmp (asm_src2, asm_src1);
            Mov (Imm 0, asm_dst);
            SetCC (cond_code, asm_dst);
          ]
      | Divide | Mod ->
          let result_reg = if op = Divide then AX else DX in
          [
            Mov (asm_src1, Reg AX);
            Cdq;
            Idiv asm_src2;
            Mov (Reg result_reg, asm_dst);
          ]
      | _ ->
          let asm_op = convert_binop op in
          [
            Mov (asm_src1, asm_dst);
            Binary { op = asm_op; src = asm_src2; dst = asm_dst };
          ])
  | FunCall { fun_name; args; dst } -> convert_function_call fun_name args dst

let pass_params param_list =
  let register_params, stack_params = ListUtil.take_drop 6 param_list in

  let pass_in_register idx param =
    let r = List.nth param_passing_regs idx in
    Assembly.Mov (Reg r, Pseudo param)
  in
  let pass_on_stack idx param =
    (* first param passed on stack has idx 0 and is passed as Stack(16) *)
    let stk = Stack (16 + (8 * idx)) in
    Assembly.Mov (stk, Pseudo param)
  in
  List.mapi pass_in_register register_params
  @ List.mapi pass_on_stack stack_params

let convert_top_level = function
  | Tacky.Function { name; global; body; params } ->
      let instructions =
        pass_params params @ List.concat_map convert_instruction body
      in
      Function { name; instructions; global }
  | Tacky.StaticVariable { name; global; init } ->
      StaticVariable { name; global; init }

let gen (Tacky.Program fn_def) = Program (List.map convert_top_level fn_def)
