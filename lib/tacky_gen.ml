module T = struct
  include Tacky (* Ensure that the Tacky module is defined or imported *)
end

let ( @:: ) xs x = xs @ [ x ]

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate
;;

let rec emit_tacky_for_exp = function
  | Ast.Constant c -> [], T.Constant c
  | Ast.Unary (op, inner) ->
    let eval_inner, variable = emit_tacky_for_exp inner in
    let dst_name = Unique_ids.make_temporary () in
    let dst = T.Var dst_name in
    let tacky_op = convert_op op in
    let instructions = eval_inner @:: T.Unary { op = tacky_op; src = variable; dst } in
    instructions, dst
;;

let emit_tacky_for_statement = function
  | Ast.Return e ->
    let eval_exp, variable = emit_tacky_for_exp e in
    eval_exp @:: T.Return variable
;;

let emit_tacky_for_function = function
  | Ast.Function { name; body } ->
    let instructions = emit_tacky_for_statement body in
    T.Function { name; body = instructions }
;;

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
