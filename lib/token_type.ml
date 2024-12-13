type t =
  | Identifier of string
  | Constant of int
  (* keywords *)
  | KWInt
  | KWVoid
  | KWReturn
  (* punctuation *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Hyphen
  | DoubleHyphen
  | Tilde
  | Semicolon
  | Plus
  | Star
  | Slash
  | Percent
  | Bang (* ! *)
  | LogicAnd (* && *)
  | LogicOr (* || *)
  | DoubleEqual (* == *)
  | BangEqual (* != *)
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | EOF
  | Invalid
[@@deriving show, eq]
