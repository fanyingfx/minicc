module T = struct
  include Token_type
end

exception LexError of char

type t = {
  content : string;
  len : int;
  mutable position : int;
  mutable ch : char option;
}

let peek_char lexer =
  if lexer.position >= lexer.len then None
  else Some (String.unsafe_get lexer.content lexer.position)

let advance_char lexer =
  let ch = peek_char lexer in
  lexer.ch <- ch;
  lexer.position <- lexer.position + 1

let from_string content =
  let lexer =
    { content; len = String.length content; position = 0; ch = None }
  in
  advance_char lexer;
  lexer

let eat_while lexer f =
  let start_pos = lexer.position - 1 in
  let rec make_str lexer =
    if f (peek_char lexer) then (
      advance_char lexer;
      make_str lexer)
    else String.sub lexer.content start_pos (lexer.position - start_pos)
  in
  make_str lexer

let make_int lexer =
  let is_digit = function Some '0' .. '9' -> true | _ -> false in
  let value = eat_while lexer is_digit in
  T.Constant (int_of_string value)

let keywords = T.[ ("int", KWInt); ("void", KWVoid); ("return", KWReturn) ]

let make_ident_or_keywords lexer =
  let is_alphabet_opt = function
    | Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> true
    | _ -> false
  in
  let idstr = eat_while lexer is_alphabet_opt in
  let kw = List.assoc_opt idstr keywords in
  match kw with Some kw -> kw | None -> T.Identifier idstr

(* skip whitespace should using lexer.ch instead of peek_char lexer *)
let rec skip_whitespace lexer =
  match lexer.ch with
  | Some (' ' | '\n' | '\r' | '\t') ->
      advance_char lexer;
      skip_whitespace lexer
  | _ -> ()

(* let make_token ch lexer *)
let make_token_lookahead ch t1 t2 lexer =
  match peek_char lexer with
  | Some c when c = ch ->
      advance_char lexer;
      t1
  | Some c when t2 = T.Invalid -> raise (LexError c)
  | _ -> t2

let next_token lexer =
  skip_whitespace lexer;
  let tok =
    match lexer.ch with
    | Some ch -> (
        match ch with
        | '0' .. '9' -> make_int lexer
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> make_ident_or_keywords lexer
        | '(' -> LParen
        | ')' -> RParen
        | '{' -> LBrace
        | '}' -> RBrace
        | ';' -> Semicolon
        | '~' -> Tilde
        | '+' -> Plus
        | '*' -> Star
        | '/' -> Slash
        | '%' -> Percent
        | '&' -> make_token_lookahead '&' T.LogicAnd T.Invalid lexer
        | '-' -> make_token_lookahead '-' T.DoubleHyphen T.Hyphen lexer
        | '|' -> make_token_lookahead '|' T.LogicOr T.Invalid lexer
        | '=' -> make_token_lookahead '=' T.DoubleEqual T.Equal lexer
        | '!' -> make_token_lookahead '=' T.BangEqual T.Bang lexer
        | '<' -> make_token_lookahead '=' T.LessOrEqual T.LessThan lexer
        | '>' -> make_token_lookahead '=' T.GreaterOrEqual T.GreaterThan lexer
        | _ -> raise (LexError ch))
    | None -> EOF
  in
  advance_char lexer;
  tok

let lex content =
  let lexer = from_string content in
  let rec lex' lexer acc =
    match next_token lexer with EOF -> acc | tok -> lex' lexer (tok :: acc)
  in
  List.rev (lex' lexer [])
