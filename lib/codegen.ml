open Assembly

let convert_exp = function
  | Ast.Constant i -> Imm i
;;

let convert_statemet = function
  | Ast.Return exp ->
    let v = convert_exp exp in
    [ Mov (v, Register); Ret ]
;;

let convert_function = function
  | Ast.Function { name; body } ->
    let instructions = convert_statemet body in
    Function { name; instructions }
;;

let gen (Ast.Program fn_def) = Program (convert_function fn_def)
