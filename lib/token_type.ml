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
| EOF
[@@deriving show, eq]