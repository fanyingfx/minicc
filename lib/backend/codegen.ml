open Assembly

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

let convert_function = function
  | Tacky.Function { name; body } ->
      let instructions = List.concat_map convert_instruction body in
      Function { name; instructions }

let gen (Tacky.Program fn_def) = Program (convert_function fn_def)
