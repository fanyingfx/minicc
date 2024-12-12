open Assembly

let convert_val = function
  | Tacky.Constant i -> Imm i
  | Tacky.Var v -> Pseudo v
;;

let convert_op = function
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg
;;

let convert_instruction = function
  | Tacky.Return tacky_val ->
    let asm_val = convert_val tacky_val in
    [ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op; dst; src } ->
    let asm_op = convert_op op
    and asm_src = convert_val src
    and asm_dst = convert_val dst in
    [ Mov ( asm_src,asm_dst); Unary (asm_op, asm_dst) ]
;;

let convert_function = function
  | Tacky.Function { name; body } ->
    let instructions = List.concat_map convert_instruction body in
    Function { name; instructions }
;;

let gen (Tacky.Program fn_def) = Program (convert_function fn_def)
