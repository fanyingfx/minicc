module T = struct
  include Token_type
end

exception ParseError of string

module Private = struct
  type expected =
    | Tok of T.t
    | Name of string

  let pp_expected fmt = function
    | Tok tk -> T.pp fmt tk
    | Name s -> Format.pp_print_string fmt s
  ;;

  let raise_error ~expected ~actual =
    let msg =
      Format.asprintf "Expected %a but found %a" pp_expected expected T.pp actual
    in
    raise (ParseError msg)
  ;;

  let expect expected tokens =
    let actual = Token_stream.take_token tokens in
    if actual <> expected then raise_error ~expected:(Tok expected) ~actual else ()
  ;;

  let parse_id tokens =
    match Token_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other
  ;;

  let parse_int tokens =
    match Token_stream.take_token tokens with
    | T.Constant x -> Ast.Constant x
    | other -> raise_error ~expected:(Name "an constant") ~actual:other
  ;;

  let parse_exp tokens = parse_int tokens

  let parse_statement tokens =
    expect T.KWReturn tokens;
    let exp = parse_exp tokens in
    expect T.Semicolon tokens;
    Ast.Return exp
  ;;

  let parse_function_definition tokens =
    expect T.KWInt tokens;
    let name = parse_id tokens in
    expect T.LParen tokens;
    expect T.KWVoid tokens;
    expect T.RParen tokens;
    expect T.LBrace tokens;
    let body = parse_statement tokens in
    Ast.Function { name; body }
  ;;

  let parse_program tokens = Ast.Program (parse_function_definition tokens)
;;
end
let parse token_list = 
  try let tokens = Token_stream.of_list token_list in
  Private.parse_program tokens
with
| Token_stream.End_of_stream -> raise (ParseError "Unexpected end of file")

  
