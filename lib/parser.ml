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

  let eat_token tokens = ignore (Token_stream.take_token tokens)

  let parse_id tokens =
    match Token_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  let is_specifier = function
    | T.KWStatic | T.KWExtern | T.KWInt -> true
    | _ -> false

  let parse_specifier tokens =
    let spec = Token_stream.take_token tokens in
    if is_specifier spec then spec
    else
      raise_error ~expected:(Name "a type or storage-class specifier")
        ~actual:spec

  let rec parse_specifier_list tokens =
    let spec = parse_specifier tokens in
    if is_specifier (Token_stream.peek tokens) then
      spec :: parse_specifier_list tokens
    else [ spec ]

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
    | T.Tilde | T.Hyphen | T.Bang ->
        let op = parse_unop tokens in
        let inner_exp = parse_factor tokens in
        Ast.Unary (op, inner_exp)
    | T.LParen ->
        ignore (Token_stream.take_token tokens);
        let inner_exp = parse_exp 0 tokens in
        expect T.RParen tokens;
        inner_exp
    | T.Identifier _ ->
        let id = parse_id tokens in
        if Token_stream.peek tokens = T.LParen then (
          eat_token tokens;
          let args =
            if Token_stream.peek tokens = T.RParen then []
            else parse_argument_list tokens
          in
          expect T.RParen tokens;
          Ast.FunCall { name = id; args })
        else Ast.Var id
    | t -> raise_error ~expected:(Name "an factor") ~actual:t

  and parse_argument_list tokens =
    let arg = parse_exp 0 tokens in
    if Token_stream.peek tokens = T.Comma then (
      eat_token tokens;
      arg :: parse_argument_list tokens)
    else [ arg ]

  and parse_exp min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = Token_stream.peek tokens in
    let rec parse_exp_loop left next =
      match get_precedence next with
      | Some prec when prec >= min_prec ->
          let left =
            if next = T.Equal then
              let _ = Token_stream.take_token tokens in
              let right = parse_exp prec tokens in
              Ast.Assignment (left, right)
            else if next = T.QuestionMark then
              let middle = parse_conditional_middle tokens in
              let right = parse_exp prec tokens in
              Ast.Conditional
                { condition = left; then_result = middle; else_result = right }
            else
              let operator = parse_binop tokens in
              let right = parse_exp (prec + 1) tokens in
              Ast.Binary (operator, left, right)
          in
          parse_exp_loop left (Token_stream.peek tokens)
      | _ -> left
    and parse_conditional_middle tokens =
      expect T.QuestionMark tokens;
      let exp = parse_exp 0 tokens in
      expect T.Colon tokens;
      exp
    in

    parse_exp_loop initial_factor next_token

  let parse_storage_class = function
    | T.KWExtern -> Ast.Extern
    | T.KWStatic -> Ast.Static
    | other ->
        raise_error ~expected:(Name "a storage class specifier") ~actual:other

  let parse_type_and_storage_class specifier_list =
    let types, storage_classes =
      List.partition (fun tok -> tok = T.KWInt) specifier_list
    in
    if List.length types <> 1 then raise (ParseError "Invalid type specifier")
    else
      let storage_class =
        match storage_classes with
        | [] -> None
        | [ sc ] -> Some (parse_storage_class sc)
        | _ :: _ -> raise (ParseError "Invalid storage class")
      in
      (Types.Int, storage_class)

  let rec parse_block_item tokens =
    if is_specifier (Token_stream.peek tokens) then
      Ast.D (parse_declaration tokens)
    else Ast.S (parse_statement tokens)

  and parse_declaration tokens =
    let specifiers = parse_specifier_list tokens in
    let _typ, storage_class = parse_type_and_storage_class specifiers in
    let name = parse_id tokens in
    match Token_stream.peek tokens with
    | T.LParen ->
        eat_token tokens;
        let params = parse_param_list tokens in
        expect T.RParen tokens;
        let body =
          match Token_stream.peek tokens with
          | T.Semicolon ->
              eat_token tokens;
              None
          | _ -> Some (parse_block tokens)
        in
        Ast.FunDecl { name; storage_class; params; body }
    | tok ->
        let init =
          if tok = T.Equal then (
            eat_token tokens;
            Some (parse_exp 0 tokens))
          else None
        in
        expect T.Semicolon tokens;
        Ast.VarDecl { name; storage_class; init }

  and parse_param_list tokens =
    if Token_stream.peek tokens = T.KWVoid then (
      eat_token tokens;
      [])
    else
      let rec param_loop () =
        expect KWInt tokens;
        let next_param = parse_id tokens in
        if Token_stream.peek tokens = T.Comma then (
          eat_token tokens;
          next_param :: param_loop ())
        else [ next_param ]
      in
      param_loop ()

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
    | T.LBrace -> Ast.Compound (parse_block tokens)
    | T.KWBreak ->
        let _ = Token_stream.take_token tokens in
        expect T.Semicolon tokens;
        Ast.Break ""
    | T.KWContinue ->
        let _ = Token_stream.take_token tokens in
        expect T.Semicolon tokens;
        Ast.Continue ""
    | T.KWWhile ->
        let _ = Token_stream.take_token tokens in
        expect LParen tokens;
        let condition = parse_exp 0 tokens in
        expect RParen tokens;
        let body = parse_statement tokens in
        Ast.While { condition; body; id = "" }
    | T.KWDo ->
        eat_token tokens;
        let body = parse_statement tokens in
        expect KWWhile tokens;
        expect LParen tokens;
        let condition = parse_exp 0 tokens in
        expect RParen tokens;
        expect Semicolon tokens;
        Ast.DoWhile { body; condition; id = "" }
    | T.KWFor ->
        eat_token tokens;
        expect LParen tokens;
        let init = parse_for_init tokens in
        let condition = parse_optional_exp T.Semicolon tokens in
        let post = parse_optional_exp T.RParen tokens in
        let body = parse_statement tokens in
        Ast.For { init; condition; post; body; id = "" }
    | _ -> (
        let opt_exp = parse_optional_exp T.Semicolon tokens in
        match opt_exp with Some e -> Ast.Expression e | None -> Ast.Null)

  and parse_for_init tokens =
    if is_specifier (Token_stream.peek tokens) then
      match parse_declaration tokens with
      | Ast.VarDecl vd -> Ast.InitDecl vd
      | _ ->
          raise (ParseError "Found a function declaration in a for loop header")
    else
      let opt_e = parse_optional_exp T.Semicolon tokens in
      Ast.InitExp opt_e

  and parse_optional_exp delim tokens =
    if Token_stream.peek tokens = delim then (
      eat_token tokens;
      None)
    else
      let e = parse_exp 0 tokens in
      expect delim tokens;
      Some e

  and parse_block tokens =
    expect T.LBrace tokens;
    let rec parse_block_loop () =
      match Token_stream.peek tokens with
      | T.RBrace -> []
      | _ ->
          let block_item = parse_block_item tokens in
          block_item :: parse_block_loop ()
    in
    let block_list = parse_block_loop () in
    expect T.RBrace tokens;
    Ast.Block block_list

  let parse_program tokens =
    let rec parse_decl_loop () =
      if Token_stream.is_empty tokens then []
      else
        let next_decl = parse_declaration tokens in
        next_decl :: parse_decl_loop ()
    in
    let func_decls = parse_decl_loop () in
    Ast.Program func_decls
end

let parse token_list =
  try
    let tokens = Token_stream.of_list token_list in
    Private.parse_program tokens
  with Token_stream.End_of_stream ->
    raise (ParseError "Unexpected end of file")
