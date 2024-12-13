module T = struct
  include Tacky
end

let ( @:: ) xs x = xs @ [ x ]

let convert_unop = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate
  | Ast.Not -> T.Not

let convert_binop = function
  | Ast.Add -> T.Add
  | Ast.Subtract -> T.Subtract
  | Ast.Multiply -> T.Multiply
  | Ast.Divide -> T.Divide
  | Ast.Mod -> T.Mod
  | Ast.Equal -> T.Equal
  | Ast.NotEqual -> T.NotEqual
  | Ast.LessThan -> T.LessThan
  | Ast.LessOrEqual -> T.LessOrEqual
  | Ast.GreaterThan -> T.GreaterThan
  | Ast.GreaterOrEqual -> T.GreaterOrEqual
  | And | Or ->
      failwith "Internal error, cannot convert these directly to TACKY binops"

let rec emit_tacky_for_exp = function
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) -> emit_unary_exp op inner
  | Ast.Binary (And, e1, e2) -> emit_and_exp e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_exp e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_exp op e1 e2

and emit_unary_exp op inner =
  let eval_inner, variable = emit_tacky_for_exp inner in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let tacky_op = convert_unop op in
  let instructions =
    eval_inner @:: T.Unary { op = tacky_op; src = variable; dst }
  in
  (instructions, dst)

and emit_binary_exp op e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let tacky_op = convert_binop op in
  let instructions =
    eval_v1
    @ eval_v2
    @:: Tacky.Binary { op = tacky_op; src1 = v1; src2 = v2; dst }
  in
  (instructions, dst)

and emit_and_exp e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let false_lable = Unique_ids.make_lable "and_false" in
  let end_lable = Unique_ids.make_lable "and_end" in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ [ T.JumpIfZero (v1, false_lable) ]
    @ eval_v2
    @ [
        T.JumpIfZero (v2, false_lable);
        T.Copy { src = Constant 1; dst };
        T.Jump end_lable;
        T.Label false_lable;
        T.Copy { src = Constant 0; dst };
        T.Label end_lable;
      ]
  in
  (instructions, dst)
and emit_or_exp e1 e2 = 
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let true_lable = Unique_ids.make_lable "or_true" in
  let end_lable = Unique_ids.make_lable "or_end" in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let instructions =
    eval_v1
    @ [ T.JumpIfNotZero (v1, true_lable) ]
    @ eval_v2
    @ [
        T.JumpIfNotZero (v2, true_lable);
        T.Copy { src = Constant 0; dst };
        T.Jump end_lable;
        T.Label true_lable;
        T.Copy { src = Constant 1; dst };
        T.Label end_lable;
      ]
  in
  (instructions, dst)

let emit_tacky_for_statement = function
  | Ast.Return e ->
      let eval_exp, variable = emit_tacky_for_exp e in
      eval_exp @:: T.Return variable

let emit_tacky_for_function = function
  | Ast.Function { name; body } ->
      let instructions = emit_tacky_for_statement body in
      T.Function { name; body = instructions }

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
