open Assembly

let convert_val = function
  | Tacky.Constant i -> Imm i
  | Tacky.Var v -> Pseudo v
;;

let convert_op = function
  | Tacky.Complement -> Not
  | Tacky.Negate -> Neg
;;
let convert_binop = function
| Tacky.Add -> Add
| Tacky.Multiply -> Mult
| Tacky.Subtract -> Sub
| Tacky.(Divide |Mod) -> 
  failwith "Internal error: shouldn't handle division like other binary operators"
    [@coverage off]

let convert_instruction = function
  | Tacky.Return tacky_val ->
    let asm_val = convert_val tacky_val in
    [ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op; dst; src } ->
    let asm_op = convert_op op
    and asm_src = convert_val src
    and asm_dst = convert_val dst in
    [ Mov ( asm_src,asm_dst); Unary (asm_op, asm_dst) ]
  | Tacky.Binary{op;src1;src2;dst} ->
    let asm_op = convert_binop op 
    and asm_src1 = convert_val src1 
    and asm_src2 = convert_val src2 
    and asm_dst = convert_val dst in 
    match op with 
    | Divide|Mod -> 
      let result_reg = if op = Divide then AX else DX in 
      [
        Mov(asm_src1,Reg AX);
        Cdq;
        Idiv asm_src2;
        Mov (Reg result_reg, asm_dst)
      ]
    |_ ->[Mov(asm_src1,asm_dst);Binary{op=asm_op;src=asm_src2;dst=asm_dst}]
;;

let convert_function = function
  | Tacky.Function { name; body } ->
    let instructions = List.concat_map convert_instruction body in
    Function { name; instructions }
;;

let gen (Tacky.Program fn_def) = Program (convert_function fn_def)
