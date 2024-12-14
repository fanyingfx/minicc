module T = struct
  include Token_type
end

exception ParseError of string

module Private = struct
  type expected = Tok of T.t | Name of string

  let pp_expected fmt = function
    | Tok tk -> T.pp fmt tk
    | Name s -> Format.pp_print_string fmt s

  let raise_error ~expected ~actual =
    let msg =
      Format.asprintf "Expected %a but found %a" pp_expected expected T.pp
        actual
    in
    raise (ParseError msg)

  let expect expected tokens =
    let actual = Token_stream.take_token tokens in
    if actual <> expected then raise_error ~expected:(Tok expected) ~actual
    else ()

  let parse_id tokens =
    match Token_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  let parse_unop tokens =
    match Token_stream.take_token tokens with
    | T.Tilde -> Ast.Complement
    | T.Hyphen -> Ast.Negate
    | T.Bang -> Ast.Not
    | other -> raise_error ~expected:(Name "an unary operator") ~actual:other

  let parse_binop tokens =
    match Token_stream.take_token tokens with
    | T.Plus -> Ast.Add
    | T.Hyphen -> Ast.Subtract
    | T.Star -> Ast.Multiply
    | T.Slash -> Ast.Divide
    | T.Percent -> Ast.Mod
    | T.LogicAnd -> Ast.And
    | T.LogicOr -> Ast.Or
    | T.DoubleEqual -> Ast.Equal
    | T.BangEqual -> Ast.NotEqual
    | T.LessThan -> Ast.LessThan
    | T.LessOrEqual -> Ast.LessOrEqual
    | T.GreaterThan -> Ast.GreaterThan
    | T.GreaterOrEqual -> Ast.GreaterOrEqual
    | other -> raise_error ~expected:(Name "a binary operator") ~actual:other

  let parse_int tokens =
    match Token_stream.take_token tokens with
    | T.Constant x -> Ast.Constant x
    | other -> raise_error ~expected:(Name "an constant") ~actual:other

  let get_precedence = function
    | T.Star | T.Slash | T.Percent -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.LessThan | T.LessOrEqual | T.GreaterThan | T.GreaterOrEqual -> Some 35
    | T.DoubleEqual | T.BangEqual -> Some 30
    | T.LogicAnd -> Some 10
    | T.LogicOr -> Some 5
    | T.QuestionMark -> Some 3
    | T.Equal -> Some 1
    | _ -> None

  let rec parse_factor tokens =
    let next_token = Token_stream.peek tokens in
    match next_token with
    | T.Constant _ -> parse_int tokens
    | T.Tilde | T.Hyphen ->
        let op = parse_unop tokens and inner_exp = parse_exp 0 tokens in
        Ast.Unary (op, inner_exp)
    | T.LParen ->
        ignore (Token_stream.take_token tokens);
        let inner_exp = parse_exp 0 tokens in
        expect T.RParen tokens;
        inner_exp
    | T.Identifier _ -> Ast.Var (parse_id tokens)
    | t -> raise_error ~expected:(Name "an expression") ~actual:t

  and parse_exp min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = Token_stream.peek tokens in
    let rec parse_exp_loop left next =
      match get_precedence next with
      | Some prec when prec >= min_prec -> (
          match next with
          | T.Equal ->
              let _ = Token_stream.take_token tokens in
              let right = parse_exp prec tokens in
              Ast.Assignment (left, right)
          | T.QuestionMark ->
              let middle = parse_conditional_middle tokens in
              let right = parse_exp prec tokens in
              Ast.Conditional
                { condition = left; then_result = middle; else_result = right }
          | _ ->
              let operator = parse_binop tokens in
              let right = parse_exp (prec + 1) tokens in
              let left = Ast.Binary (operator, left, right) in
              parse_exp_loop left (Token_stream.peek tokens))
      | _ -> left
    and parse_conditional_middle tokens =
      expect T.QuestionMark tokens;
      let exp = parse_exp 0 tokens in
      expect T.Colon tokens;
      exp
    in

    parse_exp_loop initial_factor next_token

  let rec parse_block_item tokens =
    match Token_stream.peek tokens with
    | T.KWInt -> Ast.D (parse_declaration tokens)
    | _ -> Ast.S (parse_statement tokens)

  and parse_declaration tokens =
    expect T.KWInt tokens;
    let var_name = parse_id tokens in
    let init =
      match Token_stream.take_token tokens with
      | T.Semicolon -> None
      | T.Equal ->
          let init_exp = parse_exp 0 tokens in
          expect T.Semicolon tokens;
          Some init_exp
      | other ->
          raise_error ~expected:(Name "An initializer or semicolon")
            ~actual:other
    in
    Ast.Declaration { name = var_name; init }

  and parse_statement tokens =
    match Token_stream.peek tokens with
    | T.KWReturn ->
        let _ = Token_stream.take_token tokens in
        let exp = parse_exp 0 tokens in
        expect T.Semicolon tokens;
        Ast.Return exp
    | T.Semicolon ->
        let _ = Token_stream.take_token tokens in
        Ast.Null
    | T.KWIf ->
        let _ = Token_stream.take_token tokens in
        expect T.LParen tokens;
        let condition = parse_exp 0 tokens in
        expect T.RParen tokens;
        let then_clause = parse_statement tokens
        and else_clause =
          if Token_stream.peek tokens = T.KWElse then
            let _ = Token_stream.take_token tokens in
            Some (parse_statement tokens)
          else None
        in
        Ast.If { condition; then_clause; else_clause }
    | T.LBrace ->
        let _ = Token_stream.take_token tokens in
        Ast.Compound (parse_block tokens)
    | _ ->
        let exp = parse_exp 0 tokens in
        expect T.Semicolon tokens;
        Ast.Expression exp

  and parse_block tokens =
    let rec parse_block_loop () =
      match Token_stream.peek tokens with
      | T.RBrace -> 
        []
      | _ -> 
        let block_item = parse_block_item tokens in 
        block_item:: parse_block_loop ()
    in
    let block_list = parse_block_loop () in
    expect T.RBrace tokens;
    Ast.Block block_list

  let parse_function_definition tokens =
    expect T.KWInt tokens;
    let name = parse_id tokens in
    expect T.LParen tokens;
    expect T.KWVoid tokens;
    expect T.RParen tokens;
    expect T.LBrace tokens;
    let body = parse_block tokens in

    Ast.Function { name; body }

  let parse_program tokens = Ast.Program (parse_function_definition tokens)
end

let parse token_list =
  try
    let tokens = Token_stream.of_list token_list in
    Private.parse_program tokens
  with Token_stream.End_of_stream ->
    raise (ParseError "Unexpected end of file")
