open Assembly

let fixup_instruction = function
  | Mov ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Mov (Reg R10, dst) ]
  | Idiv (Imm i) -> [ Mov (Imm i, Reg R10); Idiv (Reg R10) ]
  | Binary
      { op = (Add | Sub) as op; src = Stack _ as src; dst = Stack _ as dst } ->
      [ Mov (src, Reg R10); Binary { op; src = Reg R10; dst } ]
  | Binary { op = Mult as op; src; dst = Stack _ as dst } ->
      [
        Mov (dst, Reg R11); Binary { op; src; dst = Reg R11 }; Mov (Reg R11, dst);
      ]
  | Cmp ((Stack _ as op1),(Stack _ as op2)) ->
    [Mov (op1,Reg R10);Cmp ((Reg R10),op2)]
  | Cmp (op1,(Imm _ as imm)) ->
    [Mov (imm,Reg R11);Cmp (op1,Reg R11)]
  | other -> [ other ]

let fixup_function last_stack_slot (Function { name; instructions }) =
  Function
    {
      name;
      instructions =
        AllocateStack (-last_stack_slot)
        :: List.concat_map fixup_instruction instructions;
    }

let fixup_program last_stack_slot (Program fn_def) =
  Program (fixup_function last_stack_slot fn_def)
