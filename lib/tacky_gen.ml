module T = struct
  include Tacky
end

let ( @:: ) xs x = xs @ [ x ]
let break_label id = "break." ^ id
let continue_label id = "continue." ^ id

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
  | Ast.Var v -> ([], T.Var v)
  | Ast.Binary (And, e1, e2) -> emit_and_exp e1 e2
  | Ast.Binary (Or, e1, e2) -> emit_or_exp e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_exp op e1 e2
  | Ast.Assignment (Var v, rhs) ->
      let instructions, result = emit_tacky_for_exp rhs in
      let new_insttruction =
        instructions @:: T.Copy { src = result; dst = Var v }
      in
      (new_insttruction, T.Var v)
  | Ast.Assignment _ -> failwith "Internal error: bad lvalue"
  | Ast.Conditional { condition; then_result; else_result } ->
      emit_conditiona_exp condition then_result else_result

and emit_conditiona_exp condition then_result else_result =
  let eval_condition, cond_var = emit_tacky_for_exp condition in
  let else_label = Unique_ids.make_label "else" in
  let end_label = Unique_ids.make_label "if_end" in
  let eval_then, then_val = emit_tacky_for_exp then_result in
  let eval_else, else_val = emit_tacky_for_exp else_result in
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let instructions =
    eval_condition
    @ [ T.JumpIfZero (cond_var, else_label) ]
    @ eval_then
    @ [ T.Copy { src = then_val; dst }; T.Jump end_label; T.Label else_label ]
    @ eval_else
    @ [ T.Copy { src = else_val; dst }; T.Label end_label ]
  in
  (instructions, dst)

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
  let false_lable = Unique_ids.make_label "and_false" in
  let end_lable = Unique_ids.make_label "and_end" in
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
  let true_lable = Unique_ids.make_label "or_true" in
  let end_lable = Unique_ids.make_label "or_end" in
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

let rec emit_tacky_for_statement = function
  | Ast.Return e ->
      let eval_exp, variable = emit_tacky_for_exp e in
      eval_exp @:: T.Return variable
  | Ast.Expression exp ->
      let eval_exp, _variable = emit_tacky_for_exp exp in
      eval_exp
  | Ast.If { condition; then_clause; else_clause } ->
      emit_tacky_for_if_statement condition then_clause else_clause
  | Ast.DoWhile { condition; body; id } ->
      emit_tacky_for_doloop body condition id
  | Ast.While { condition; body; id } ->
      emit_tacky_for_while_loop condition body id
  | Ast.For { init; condition; post; body; id } ->
      emit_tacky_for_for_loop init condition post body id
  | Ast.Compound (Block block) -> emit_tacky_for_block block
  | Ast.Break id -> [ T.Jump (break_label id) ]
  | Ast.Continue id -> [ T.Jump (continue_label id) ]
  | Ast.Null -> []

and emit_tacky_for_if_statement condition then_clause = function
  | None ->
      let eval_condition, cond_val = emit_tacky_for_exp condition in
      let end_label = Unique_ids.make_label "if_end" in
      let eval_then = emit_tacky_for_statement then_clause in
      eval_condition
      @ (T.JumpIfZero (cond_val, end_label) :: eval_then)
      @:: T.Label end_label
  | Some else_clause ->
      let eval_condition, cond_val = emit_tacky_for_exp condition in
      let else_label = Unique_ids.make_label "else" in
      let end_label = Unique_ids.make_label "if_end" in
      let eval_then = emit_tacky_for_statement then_clause in
      let eval_else = emit_tacky_for_statement else_clause in
      eval_condition
      @ (T.JumpIfZero (cond_val, else_label) :: eval_then)
      @ [ T.Jump end_label; T.Label else_label ]
      @ eval_else
      @:: T.Label end_label

and emit_tacky_for_doloop body condition id =
  let start_label = Unique_ids.make_label "do_loop_start" in
  let cont_label = continue_label id in
  let break_label = break_label id in
  let eval_condition, cond_val = emit_tacky_for_exp condition in
  let eval_body = emit_tacky_for_statement body in
  (T.Label start_label :: eval_body)
  @ (T.Label cont_label :: eval_condition)
  @ [ T.JumpIfNotZero (cond_val, start_label); Label break_label ]

and emit_tacky_for_while_loop condition body id =
  let cont_label = continue_label id in
  let break_label = break_label id in
  let eval_condition, cond_val = emit_tacky_for_exp condition in
  let eval_body = emit_tacky_for_statement body in
  (T.Label cont_label :: eval_condition)
  @ [ T.JumpIfZero (cond_val, break_label) ]
  @ eval_body
  @ [ T.Jump cont_label; Label break_label ]

and emit_tacky_for_for_loop init condition post body id =
  let start_label = Unique_ids.make_label "for_loop_start"
  and cont_label = continue_label id
  and break_label = break_label id in
  let eval_init =
    match init with
    | InitDecl d -> emit_declaration d
    | InitExp None -> []
    | InitExp (Some exp) -> fst (emit_tacky_for_exp exp)
  and eval_condition =
    match Option.map emit_tacky_for_exp condition with
    | Some (insts, v) -> insts @ [ T.JumpIfZero (v, break_label) ]
    | None -> []
  and eval_post =
    match post with Some exp -> fst (emit_tacky_for_exp exp) | None -> []
  and eval_body = emit_tacky_for_statement body in
  eval_init
  @ (T.Label start_label :: eval_condition)
  @ eval_body
  @ (T.Label cont_label :: eval_post)
  @ [ T.Jump start_label; T.Label break_label ]

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D d -> emit_declaration d

and emit_declaration = function
  | Ast.Declaration { name; init = Some e } ->
      let eval_assignemnt, _assignment =
        emit_tacky_for_exp (Ast.Assignment (Var name, e))
      in
      eval_assignemnt
  | Ast.Declaration { init = None; _ } -> []

and emit_tacky_for_block block = List.concat_map emit_tacky_for_block_item block

let emit_tacky_for_function = function
  | Ast.Function { name; body = Block block } ->
      let body_instructions = List.concat_map emit_tacky_for_block_item block in
      let extra_return = T.(Return (Constant 0)) in
      T.Function { name; body = body_instructions @:: extra_return }

let gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
