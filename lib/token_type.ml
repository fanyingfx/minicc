type t =
  | Identifier of string
  | Constant of int
  (* keywords *)
  | KWInt
  | KWVoid
  | KWReturn
  | KWIf
  | KWElse
  | KWDo
  | KWWhile
  | KWFor
  | KWBreak
  | KWContinue
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
  | Equal
  | Bang (* ! *)
  | LogicAnd (* && *)
  | LogicOr (* || *)
  | DoubleEqual (* == *)
  | BangEqual (* != *)
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | QuestionMark
  | Colon
  | EOF
  | Invalid
[@@deriving show, eq]
