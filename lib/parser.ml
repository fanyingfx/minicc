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
    | t -> raise_error ~expected:(Name "an expression") ~actual:t

  and parse_exp min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = Token_stream.peek tokens in
    let rec parse_exp_loop left next =
      match get_precedence next with
      | Some prec when prec >= min_prec ->
          let operator = parse_binop tokens in
          let right = parse_exp (prec + 1) tokens in
          let left = Ast.Binary (operator, left, right) in
          parse_exp_loop left (Token_stream.peek tokens)
      | _ -> left
    in
    parse_exp_loop initial_factor next_token

  let parse_statement tokens =
    expect T.KWReturn tokens;
    let exp = parse_exp 0 tokens in
    expect T.Semicolon tokens;
    Ast.Return exp

  let parse_function_definition tokens =
    expect T.KWInt tokens;
    let name = parse_id tokens in
    expect T.LParen tokens;
    expect T.KWVoid tokens;
    expect T.RParen tokens;
    expect T.LBrace tokens;
    let body = parse_statement tokens in
    Ast.Function { name; body }

  let parse_program tokens = Ast.Program (parse_function_definition tokens)
end

let parse token_list =
  try
    let tokens = Token_stream.of_list token_list in
    Private.parse_program tokens
  with Token_stream.End_of_stream ->
    raise (ParseError "Unexpected end of file")
