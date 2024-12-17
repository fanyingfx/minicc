open Assembly

let fixup_instruction = function
  | Mov (((Stack _ | Data _) as src), ((Stack _ | Data _) as dst)) ->
      [ Mov (src, Reg R10); Mov (Reg R10, dst) ]
  | Idiv (Imm i) -> [ Mov (Imm i, Reg R10); Idiv (Reg R10) ]
  | Binary
      {
        op = (Add | Sub) as op;
        src = (Stack _ | Data _) as src;
        dst = (Stack _ | Data _) as dst;
      } ->
      [ Mov (src, Reg R10); Binary { op; src = Reg R10; dst } ]
  | Binary { op = Mult as op; src; dst = (Stack _ | Data _) as dst } ->
      [
        Mov (dst, Reg R11); Binary { op; src; dst = Reg R11 }; Mov (Reg R11, dst);
      ]
  | Cmp (((Stack _ | Data _) as op1), ((Stack _ | Data _) as op2)) ->
      [ Mov (op1, Reg R10); Cmp (Reg R10, op2) ]
  | Cmp (op1, (Imm _ as imm)) -> [ Mov (imm, Reg R11); Cmp (op1, Reg R11) ]
  | other -> [ other ]

let fixup_top_level = function
  | Function { name; global; instructions } ->
      let stack_bytes = -Symbols.get_bytes_required name in
      Function
        {
          name;
          global;
          instructions =
            AllocateStack (Utils.round_away_from_zero 16 stack_bytes)
            :: List.concat_map fixup_instruction instructions;
        }
  | static_var -> static_var

let fixup_program (Program tpls) =
  let fixed_functions = List.map fixup_top_level tpls in
  Program fixed_functions
